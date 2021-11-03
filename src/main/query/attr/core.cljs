(ns query.attr.core
  (:require [roam.datascript :as rd]
            [clojure.string :as str]
            [query.attr.value :refer [ref-type
                                      parse-attribute-value
                                      attr-value->value
                                      attr-value->type]]
            [query.attr.operation :refer [operations->datalog-pred
                                          resolve-operation
                                          get-operator]]
            [query.util :refer [ref->ref-content
                                filter-query-blocks
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

(defonce attr-values
  '[(attr-values ?block ?attribute ?parse-attribute-value ?v)
    [?block :attrs/lookup ?l]
    [?l :block/refs ?attribute]
    (or-join
     [?l ?attribute ?v ?parse-attribute-value]
     ; One-liner attribute
     (and [(missing? $ ?l :block/children)]
          [?l :block/refs ?refs]
          [?l :block/string ?v]
          ; Hacky parsing to isolate value
          [?attribute :node/title ?attr-title]
          [?l :block/string ?str]
          [(str ?attr-title "::") ?roam-attr]
          [(clojure.string/starts-with? ?str ?roam-attr)]
          [(count ?attr-title) ?attr-title-len]
          [(+ ?attr-title-len 2) ?roam-attr-len-no-space]
          [(subs ?v ?roam-attr-len-no-space) ?v]
          [(subs ?v 0 1) ?first-char]
          (or [(!= ?first-char " ")]
              [(subs ?v 1) ?v])
          [(?parse-attribute-value ?v ?attribute ?refs) ?v])
     ; Multi-value attribute
     (and (not [(missing? $ ?l :block/children)])
          [?l :block/children ?children]
          [?children :block/string ?v]
          (not [(re-matches #"^\s*$" ?v)])
          [(get-else $ ?children :block/refs []) ?refs]
          [(?parse-attribute-value ?v ?attribute ?refs) ?v]))])

(defn- identity-aggregate [values]
  values)

(defn execute-roam-attr-query [current-blocks attribute operations]
  (let [operations-pred (operations->datalog-pred operations)
        where-clauses '[[?block :attrs/lookup ?attribute]
                        (attr-values ?block ?attribute ?parse-attribute-value ?v)]
        query (into '[:find ?block (aggregate ?aggr ?v)
                      :in $ % ?attribute ?parse-attribute-value ?aggr
                      :where]
                    (filter-query-blocks where-clauses))
        blocks (rd/q (add-current-blocks-to-query current-blocks query)
                     [attr-values] attribute parse-attribute-value identity-aggregate current-blocks)]
    (->> blocks
         (filter #(operations-pred (second %)))
         (map first))))

(defn roam-attr-query [current-blocks block children]
  (let [refs (block :block/refs)]
    (if (seq children)
      (let [attr-ref (:db/id (first refs))]
        (execute-roam-attr-query current-blocks
                                 attr-ref
                                 [(resolve-operation attr-ref children)]))
      (let [attr-title (first (str/split (block :block/string) #"::"))
            attr-ref (:db/id (first (filter #(= (% :node/title) attr-title) refs)))
            operation [(get-operator :=)
                       [[(:db/id (first (filter #(not= % attr-ref) refs)))
                         ref-type]]]]
        (execute-roam-attr-query current-blocks attr-ref [operation])))))

(defn- execute-block-datomic-query [current-blocks ref datomic-attr]
  (let [query '[:find [?block ...]
                :in $ ?ref ?datomic-attr
                :where
                [?ref ?datomic-attr ?block]]]
    (rd/q (add-current-blocks-to-query current-blocks query)
          ref datomic-attr current-blocks)))

(defn- ref-datomic-attr-query? [block]
  (let [block-string (str/trim (block :block/string))
        split-str (str/split block-string #" ")]
    (and (>= 1 (count (block :block/refs)))
         (boolean (some #{(keyword (subs (last split-str) 1))} block-datomic-attrs)))))

(defn- find-longest-ref [refs]
  (reduce #(cond (= %1 nil) %1
                 (> (count (%2 :node/title)) (count (%1 :node/title))) %2
                 :else %1) nil refs))

(defn- ref-datomic-query [current-blocks block]
  ; A hack to ensure we retrieve [[Test [[A]]]] instead of [[A]]
  (let [ref (find-longest-ref (block :block/refs))
        datomic-attr (extract-datomic-attr (block :block/string))]
    (execute-block-datomic-query current-blocks ref datomic-attr)))

(defn execute-reverse-roam-attr-query [current-blocks attr-ref input-ref]
  (let [results (->>
                 (rd/q '[:find [?block ...]
                         :in $ ?attr-ref ?input-ref ?parse-attribute-value %
                         :where
                         (attr-values ?input-ref ?attr-ref ?parse-attribute-value ?block)]
                       attr-ref
                       input-ref
                       parse-attribute-value
                       [attr-values])
                 (reduce #(if (= (attr-value->type %2) ref-type)
                            (conj %1 (attr-value->value %2))
                            %1)
                         []))
        blocks-set (set current-blocks)]
    (if (seq current-blocks)
      (vec (filter #(contains? blocks-set %) results))
      results)))

(defn- reverse-roam-attr-query [current-blocks block]
  (let [attr-title (-> block
                       (:block/string)
                       (str/split #" ")
                       (first)
                       (str/trim)
                       (subs 1)
                       (ref->ref-content))
        block-refs (block :block/refs)
        attr-ref (first (filter #(= (% :node/title) attr-title) block-refs))
        input-ref (filter #(not= attr-ref %) block-refs)]
    (execute-reverse-roam-attr-query current-blocks attr-ref input-ref)))

(defn- roam-attr-query? [block-string]
  (str/includes? block-string "::"))

(defn- reverse-roam-attr-query? [block-string]
  (str/starts-with? (str/trim block-string) ":"))

(defn attr-query? [block]
  (let [block-string (str/trim (block :block/string))]

    (or (roam-attr-query? block-string)
        (ref-datomic-attr-query? block))))

(defn attr-query [current-blocks clause-block clause-children]
  (let [block-string (str/trim (clause-block :block/string))]
    (cond (roam-attr-query? block-string) (roam-attr-query current-blocks clause-block clause-children)
          (reverse-roam-attr-query? block-string) (reverse-roam-attr-query current-blocks clause-block)
          (ref-datomic-attr-query? clause-block) (ref-datomic-query current-blocks clause-block))))
