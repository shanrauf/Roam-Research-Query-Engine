(ns query.attr.core
  (:require [roam.datascript :as rd]
            [clojure.string :as str]
            [query.attr.value :refer [ref-type
                                      parse-attribute-value
                                      attr-value->value
                                      attr-value->type]]
            [query.attr.operation :refer [operations->-predicate
                                          resolve-operation
                                          get-operator
                                          equality-operation?]]
            [query.util :refer [ref->ref-content
                                filter-query-blocks
                                ref-length
                                add-current-blocks-to-query]]))

(defonce block-datomic-attrs
  {:attrs/lookup :attrs/_lookup
   :block/children :block/_children
   :block/page :block/_page
   :block/parents :block/_parents
   :block/refs :block/_refs})

(defn- extract-datomic-attr [block-string]
  (-> (str/split block-string #" ")
      (first)
      (subs 1)
      (keyword)))

(defn- single-value-attr? [block-string]
  (not (-> (str/trim block-string)
           (str/ends-with? "::"))))

(defonce attr-values-rule
  '[(attr-values ?block ?attr-ref ?v ?is-single-value ?parse-attribute-value)
    [?block :attrs/lookup ?attr-block]
    [?attr-block :block/refs ?attr-ref]
    [?attr-block :block/string ?attr-string]
    (or-join
     [?attr-block ?attr-ref ?v ?attr-string ?is-single-value ?parse-attribute-value]
     ; One-liner attribute
     (and [(?is-single-value ?attr-string)]
          [?attr-block :block/refs ?refs]
          [?attr-block :block/string ?v]
          ; Hacky parsing to isolate value
          [?attr-ref :node/title ?attr-title]
          [?attr-block :block/string ?str]
          [(str ?attr-title "::") ?roam-attr]
          [(clojure.string/starts-with? ?str ?roam-attr)]
          [(count ?attr-title) ?attr-title-len]
          [(+ ?attr-title-len 2) ?roam-attr-len-no-space]
          [(subs ?v ?roam-attr-len-no-space) ?v]
          [(subs ?v 0 1) ?first-char]
          (or [(!= ?first-char " ")]
              [(subs ?v 1) ?v])
          [(?parse-attribute-value ?v ?attr-ref ?refs) ?v])
     ; Multi-value attribute
     (and (not [(?is-single-value ?attr-string)])
          ; Assume multi-value (Roam doesn't add empty attrs to :attrs/lookup)
          [?attr-block :block/children ?children]
          [?children :block/string ?v]
          (not [(re-matches #"^\s*$" ?v)])
          [(get-else $ ?children :block/refs []) ?refs]
          [(?parse-attribute-value ?v ?attr-ref ?refs) ?v]))])

(defn- identity-aggregate
  "Aggregate that forces Datascript to aggregate a
   block's attribute values into a vector. This is
   literally 125x faster than [?v ...] subqueries."
  [values]
  values)

(defn eval-roam-attr-query
  [current-blocks attribute operations input-refs]
  (let [operations-pred (operations->-predicate operations)
        ; Optimization: Only search/parse blocks that have
        ; all input refs in :attrs/lookup
        lookup-clauses (into '[[?block :attrs/lookup ?attribute]]
                             (if (seq input-refs)
                               '[[(ground ?input-refs) [?input-ref ...]]
                                 [?block :attrs/lookup ?input-ref]]
                               []))
        where-clauses (-> (into lookup-clauses
                                '[(attr-values ?block ?attribute ?v ?is-single-value ?parse-attribute-value)])
                          (filter-query-blocks))
        query (into '[:find ?block (aggregate ?aggr ?v)
                      :in $ % ?attribute ?parse-attribute-value ?aggr ?is-single-value ?input-refs
                      :where]
                    where-clauses)
        block-val-pairs (rd/q (add-current-blocks-to-query current-blocks query)
                              [attr-values-rule] attribute parse-attribute-value identity-aggregate single-value-attr? input-refs current-blocks)]
    (->> block-val-pairs
         (filter #(operations-pred (second %)))
         (mapv first))))

(defn- ref-equality-check? [attr-values operation]
  (and (map #(= (attr-value->type %)
                ref-type) attr-values)
       (equality-operation? operation)))

(defn roam-attr-query [current-blocks block children]
  (let [refs (block :block/refs)]
    (if (seq children)
      (let [attr-ref (:db/id (first refs))
            operation (resolve-operation attr-ref children)
            attr-values (second operation)
            input-refs (if (ref-equality-check? attr-values operation)
                         (map attr-value->value attr-values)
                         nil)]
        (eval-roam-attr-query current-blocks
                              attr-ref
                              [operation]
                              input-refs))
      (let [attr (str/split (block :block/string) #"::")
            attr-title (first attr)
            attr-ref (:db/id (first (filter #(= (% :node/title) attr-title) refs)))
            input-ref (first (filter #(not= % attr-ref) (map :db/id refs)))
            attr-value (-> (str/join "" (rest attr))
                           (parse-attribute-value attr-ref input-ref))
            operation [(get-operator :=)
                       [attr-value]]
            input-refs (if (ref-equality-check? attr-value operation)
                         [input-ref]
                         nil)]
        (eval-roam-attr-query current-blocks attr-ref [operation] input-refs)))))

(defn eval-ref-datomic-query [current-blocks ref datomic-attr]
  (let [query (->> '[:find [?block ...]
                     :in $ ?ref ?datomic-attr
                     :where
                     [?ref ?datomic-attr ?block]]
                   (add-current-blocks-to-query current-blocks))]
    (rd/q query ref datomic-attr current-blocks)))

(defn- ref-datomic-attr-query? [block]
  (and (>= 1 (count (block :block/refs)))
       (contains? block-datomic-attrs
                  (-> (block :block/string)
                      (str/trim)
                      (str/split #" ")
                      (last)
                      (subs 1)
                      (keyword)))))

(defn- find-longest-ref [refs]
  (reduce #(cond (= %1 nil) %1
                 (> (count (%2 :node/title))
                    (count (%1 :node/title))) %2
                 :else %1) nil refs))

(defn- ref-datomic-query [current-blocks block]
  ; A hack to retrieve [[Test [[A]]]] instead of [[A]]
  ; TODO would it be faster/cleaner to just use ref-length?
  (let [ref (find-longest-ref (block :block/refs))
        datomic-attr (extract-datomic-attr (block :block/string))]
    (eval-ref-datomic-query current-blocks ref datomic-attr)))

(defn eval-reverse-roam-attr-query [current-blocks attr-ref input-ref]
  (let [results (->> (rd/q '[:find [?block ...]
                             :in $ ?attr-ref ?input-ref ?parse-attribute-value % ?is-single-value
                             :where
                             (attr-values ?input-ref ?attr-ref ?block ?is-single-value ?parse-attribute-value)]
                           attr-ref
                           input-ref
                           parse-attribute-value
                           [attr-values-rule]
                           single-value-attr?)
                     (reduce #(if (= (attr-value->type %2) ref-type)
                                (conj %1 (attr-value->value %2))
                                %1)
                             []))
        current-blocks-set (set current-blocks)]
    (if (seq current-blocks)
      (vec (filter #(contains? current-blocks-set %) results))
      results)))

(defn- reverse-roam-attr-query [current-blocks block]
  (let [block-string (-> block
                         (:block/string)
                         (str/trim))
        [_ attr-ref-len] (-> block-string
                             (subs 1)
                             (ref-length))
        attr-title (-> block-string
                       (subs 1 (+ 1 attr-ref-len))
                       (ref->ref-content))
        block-refs (block :block/refs)
        attr-ref (:db/id (first (filter #(= (% :node/title) attr-title) block-refs)))
        input-ref (first (filter #(not= attr-ref %) (map :db/id block-refs)))]
    (eval-reverse-roam-attr-query current-blocks attr-ref input-ref)))

(defn- roam-attr-query? [block-string]
  (str/includes? block-string "::"))

(defn- reverse-roam-attr-query? [block-string]
  (str/starts-with? (str/trim block-string) ":"))

(defn attr-query? [block]
  (let [block-string (str/trim (block :block/string))]
    (or (roam-attr-query? block-string)
        (reverse-roam-attr-query? block-string)
        (ref-datomic-attr-query? block))))

(defn attr-query [current-blocks clause-block clause-children]
  (let [block-string (str/trim (clause-block :block/string))]
    (cond (roam-attr-query? block-string)
          (roam-attr-query current-blocks clause-block clause-children)

          (reverse-roam-attr-query? block-string)
          (reverse-roam-attr-query current-blocks clause-block)

          (ref-datomic-attr-query? clause-block)
          (ref-datomic-query current-blocks clause-block))))
