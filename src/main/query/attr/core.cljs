(ns query.attr.core
  (:require [roam.datascript :as rd]
            [clojure.string :as str]
            [datascript.core]
            [query.attr.value :refer [ref-type
                                      text-type
                                      num-type
                                      extract-attr-values
                                      attr-value->value
                                      attr-value->type]]
            [query.attr.operation :refer [filter-by-operations
                                          passes-operations?
                                          resolve-operation
                                          get-operator
                                          one-line-query-operators
                                          some-attr-values-satisfy-generic-clause-op
                                          equality-operation?]]
            [query.util :refer [ref->ref-content
                                filter-query-blocks
                                ref-length
                                datomic-attrs
                                block-datomic-attrs
                                text-datomic-attrs
                                generic-query-eid?
                                num-datomic-attrs
                                reverse->block-datomic-attrs
                                add-current-blocks-to-query]]))

(defn- extract-datomic-attr [block-string]
  (->> (str/split block-string #" ")
       (reduce #(let [attr (-> (subs %2 1)
                               (keyword))]
                  (if (contains? datomic-attrs attr)
                    (reduced attr)
                    %1)) nil)))

(defn- reverse-datomic-attr? [attr]
  (contains? reverse->block-datomic-attrs attr))

(defn- flip-datomic-attr [attr]
  (get reverse->block-datomic-attrs attr))

(defn single-value-attr? [block-string]
  (not (-> (str/trim block-string)
           (str/ends-with? "::"))))

(defn- eid->block-refs [eid]
  (mapv :db/id (get (rd/entity eid) :block/refs)))

(defn parse-one-line-roam-attr [attr-str]
  (let [attr (str/split attr-str #"::")
        attr-title (first attr)
        attr-content (str/trim (str/join "" (rest attr)))]
    [attr-title attr-content]))

(defn extract-attr-ref [attr-title refs]
  (:db/id (first (filter #(= (% :node/title) attr-title) refs))))

(def attr-values-rule
  '[(attr-values ?block ?attr-eid ?v ?is-single-value ?parse-one-line-attr ?extract-attr-values ?eid->block-refs)
    [?block :attrs/lookup ?attr-block]
    [?attr-block :block/refs ?attr-eid]
    [?attr-block :block/string ?attr-string]
    [(?is-single-value ?attr-string) ?single-value]
    (or-join
     [?attr-block ?attr-eid ?v ?attr-string ?single-value ?parse-one-line-attr ?extract-attr-values ?eid->block-refs]
     ; One-liner attribute
     (and [(true? ?single-value)]
          [(?parse-one-line-attr ?attr-string) [_ ?v]]
          [(?eid->block-refs ?attr-block) ?refs]
          [(?extract-attr-values ?v ?attr-eid ?refs) ?v])
     ; Multi-value attribute
     (and (not [(true? ?single-value)])
          [?attr-block :block/children ?children]
          [?children :block/string ?v]
          ; Ignore empty blocks (even though Roam adds them to :attrs/lookup)
          (not [(re-matches #"^\s*$" ?v)])
          [(?eid->block-refs ?children) ?refs]
          [(?extract-attr-values ?v ?attr-eid ?refs) ?v]))
    [(ground ?v) [?v ...]]])

(defn identity-aggregate
  "Aggregate that forces Datascript to aggregate a
   block's attribute values into a vector. This is
   literally 125x faster than [?v ...] subqueries."
  [values]
  values)

(defn eval-roam-attr-query [blocks attr]
  (let [query (-> '[:find ?block (aggregate ?identity-fn ?v)
                    :in $ % ?attr ?parse-one-line-attr-fn ?extract-attr-values-fn ?identity-fn ?is-single-value-fn ?eid->block-refs-fn
                    :where]
                  (into (-> '[(attr-values ?block ?attr ?v ?is-single-value-fn ?parse-one-line-attr-fn ?extract-attr-values-fn ?eid->block-refs-fn)]
                            (filter-query-blocks)))
                  (add-current-blocks-to-query blocks))]
    (rd/q query
          [attr-values-rule] attr parse-one-line-roam-attr extract-attr-values identity-aggregate single-value-attr? eid->block-refs blocks)))

(defn execute-roam-attr-query [blocks attr operations]
  (->> (eval-roam-attr-query blocks
                             attr)
       (filter-by-operations operations)
       (mapv first)))

(defn- ref-equality-check? [attr-values operation]
  (and (every? #(= (attr-value->type %)
                   ref-type) attr-values)
       (equality-operation? operation)))

(defn roam-attr-query [current-blocks block eval-generic-roam-query]
  (let [block-refs (:block/refs block)
        children (->> (:block/children block)
                      (sort-by :block/order))]
    (if (seq children)
      (let [attr-ref (:db/id (first block-refs))
            operation (resolve-operation attr-ref children eval-generic-roam-query)]
        (execute-roam-attr-query current-blocks attr-ref [operation]))
      (let [[attr-title attr-content] (parse-one-line-roam-attr (:block/string block))
            attr-ref (extract-attr-ref attr-title block-refs)
            refs (filter #(not= % attr-ref) (map :db/id block-refs))
            input-lower (str/lower-case attr-content)]
        (if (contains? one-line-query-operators (keyword input-lower))
          (execute-roam-attr-query current-blocks attr-ref [[(get-operator input-lower)
                                                             []]])
          (let [attr-values (extract-attr-values attr-content
                                                 attr-ref
                                                 refs)
                first-ref (first refs)
                is-generic-query-ref (generic-query-eid? first-ref)
                operation (if is-generic-query-ref
                            [(some-attr-values-satisfy-generic-clause-op (:block/uid first-ref) eval-generic-roam-query)
                             []]
                            [(get-operator :includes)
                             attr-values])]
            (execute-roam-attr-query current-blocks attr-ref [operation])))))))

(defn eval-reverse-roam-attr-query [current-blocks attr-ref input-ref]
  (let [results (->> (rd/q '[:find [?block ...]
                             :in $ ?attr-ref ?input-ref ?extract-attr-values % ?parse-one-line-attr ?is-single-value ?eid->block-refs
                             :where
                             (attr-values ?input-ref ?attr-ref ?block ?is-single-value ?parse-one-line-attr ?extract-attr-values ?eid->block-refs)]
                           attr-ref
                           input-ref
                           extract-attr-values
                           [attr-values-rule]
                           parse-one-line-roam-attr
                           single-value-attr?
                           eid->block-refs)
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

(defn reverse-roam-attr-query? [block-string]
  (str/starts-with? (str/trim block-string) ":[["))

(defn- datomic-attr-query? [block]
  (not (nil? (-> (block :block/string)
                 (str/trim)
                 (extract-datomic-attr)))))

(defn- parse-datomic-attr-values [attr values]
  (cond (contains? block-datomic-attrs attr)
        (map #(identity [% ref-type]) values)

        (contains? num-datomic-attrs attr)
        (map #(identity [% num-type]) values)

        (contains? text-datomic-attrs attr)
        (map #(identity [% text-type]) values)

        :else (throw (js/Error. (str "Unknown attribute: " attr)))))

(defn eval-datomic-attr-query [current-blocks datomic-attr operations input-refs]
  (let [query (-> '[:find ?block (aggregate ?identity ?v)
                    :in $ ?datomic-attr ?identity ?input-refs
                    :where]
                  (into (-> '[[?block ?datomic-attr ?v]]
                            (filter-query-blocks)))
                  (add-current-blocks-to-query current-blocks))]
    (->> (rd/q query
               datomic-attr identity-aggregate input-refs current-blocks)
         (filter #(-> (parse-datomic-attr-values datomic-attr (second %))
                      (passes-operations? operations)))
         (mapv first))))

(defn eval-reverse-datomic-attr-query [current-blocks datomic-attr input-refs]
  (rd/q (-> '[:find [?block ...]
              :in $ ?datomic-attr ?input-refs
              :where]
            (into (-> '[[(ground ?input-refs) [?input-ref ...]]
                        [?input-ref ?datomic-attr ?block]]
                      (filter-query-blocks)))
            (add-current-blocks-to-query current-blocks))
        (flip-datomic-attr datomic-attr) input-refs current-blocks))

(defn datomic-attr-query [current-blocks block eval-generic-roam-query]
  (let [children (->> (:block/children block)
                      (sort-by :block/order))
        datomic-attr (extract-datomic-attr (:block/string block))]
    (if (seq children)
      (let [operation (resolve-operation datomic-attr children eval-generic-roam-query)
            attr-values (second operation)
            input-refs (if (ref-equality-check? attr-values operation)
                         (map attr-value->value attr-values)
                         nil)]
        (eval-datomic-attr-query current-blocks
                                 datomic-attr
                                 [operation]
                                 input-refs))
      (let [block-string (:block/string block)
            datomic-attr (extract-datomic-attr block-string)
            refs (mapv :db/id (:block/refs block))
            str-content (str/trim (str/replace block-string (str datomic-attr) ""))
            str-lower (str/lower-case str-content)]
        (if (contains? one-line-query-operators (keyword str-lower))
          (eval-datomic-attr-query current-blocks
                                   datomic-attr
                                   [[(get-operator str-lower)
                                     []]]
                                   [])
          (let [attr-values (extract-attr-values str-content datomic-attr refs)
                first-ref (first refs)
                is-generic-query-ref (generic-query-eid? first-ref)
                operation (if is-generic-query-ref
                            [(some-attr-values-satisfy-generic-clause-op (:block/uid first-ref) eval-generic-roam-query)
                             []]
                            [(get-operator :includes)
                             attr-values])
                input-refs (if (and (not is-generic-query-ref)
                                    (ref-equality-check? attr-values operation))
                             refs
                             nil)]
            (if (reverse-datomic-attr? datomic-attr)
              (eval-reverse-datomic-attr-query current-blocks datomic-attr input-refs)
              (eval-datomic-attr-query current-blocks datomic-attr [operation] input-refs))))))))

(defn attr-query? [block]
  (let [block-string (str/trim (block :block/string))]
    (or (roam-attr-query? block-string)
        (reverse-roam-attr-query? block-string)
        (datomic-attr-query? block))))

(defn attr-query [current-blocks clause-block eval-generic-roam-query]
  (let [block-string (str/trim (clause-block :block/string))]
    (cond (roam-attr-query? block-string)
          (roam-attr-query current-blocks clause-block eval-generic-roam-query)

          (reverse-roam-attr-query? block-string)
          (reverse-roam-attr-query current-blocks clause-block)

          (datomic-attr-query? clause-block)
          (datomic-attr-query current-blocks clause-block eval-generic-roam-query))))
