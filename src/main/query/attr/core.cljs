(ns query.attr.core
  (:require [roam.datascript :as rd]
            [clojure.string :as str]
            [query.attr.value :refer [ref-type parse-attribute-value]]
            [query.attr.operation :refer [operations->datalog-pred
                                          resolve-operation
                                          get-operator]]))

(defonce block-datomic-attrs
  [:attrs/lookup
   :attrs/_lookup
   :block/children
   :block/_children
   :block/page
   :block/_page
   :block/parents
   :block/_parents
   :block/refs
   :block/_refs])

(defn- extract-datomic-attr [block-string]
  (-> (str/split block-string #" ")
      (first)
      (subs 1)
      (keyword)))

(defn- datomic-attr-query? [block-string]
  (let [split-str (str/split block-string #" ")]
    (and (= 1 (count split-str))
         (boolean (some #{(extract-datomic-attr block-string)} block-datomic-attrs)))))

(defn execute-datomic-attr-query [datomic-attr operations]
  (rd/q '[:find [?block ...]
          :in $ ?datomic-attr ?operations-pred
          :where
          [(?rdq [:find [?v ...]
                  :in $ ?block ?datomic-attr
                  :where
                  [?block ?datomic-attr ?v]] ?block ?datomic-attr) ?attr-values]
          [(operations-pred ?attr-values)]]
        datomic-attr (operations->datalog-pred operations)))

(defn- datomic-attr-query [block children]
  (let [datomic-attr (extract-datomic-attr (block :block/string))
        operator-value-pair (resolve-operation datomic-attr children)]
    (execute-datomic-attr-query datomic-attr operator-value-pair)))

(defn execute-roam-attr-query [attribute operations]
  (let [operations-pred (operations->datalog-pred operations)]
    (rd/q '[:find [?block ...]
            :in $ ?attribute ?rdq ?parse-attribute-value ?operations-pred
            :where
            [?block :attrs/lookup ?attribute]
            [(?rdq [:find [?v ...]
                    :in $ ?block ?attribute ?parse-attribute-value
                    :where
                    [?block :attrs/lookup ?l]
                    (or-join
                     [?l ?attribute ?v ?parse-attribute-value]
                     ; Multi-value attribute
                     (and [?l :block/parents ?parent]
                          [?parent :block/refs ?attribute]
                          [?parent :block/children ?l]
                          [?l :block/string ?v]
                          (not [(re-matches #"^\s*$" ?v)])
                          [(get-else $ ?l :block/refs []) ?refs]
                          [(?parse-attribute-value ?v ?attribute ?refs) ?v])
                     ; One-liner attribute
                     (and [?l :block/refs ?attribute]
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
                          [(?parse-attribute-value ?v ?attribute ?refs) ?v]))]
                   ?block ?attribute ?parse-attribute-value)
             ?values]
            [(?operations-pred ?values)]]
          attribute rd/q parse-attribute-value operations-pred)))

(defn roam-attr-query [block children]
  (let [refs (block :block/refs)]
    (if (contains? block :block/children)
      (let [attr-ref (first refs)]
        (execute-roam-attr-query attr-ref
                                 [(resolve-operation attr-ref children)]))
      (let [attr-title (first (str/split (block :block/string) #"::"))
            attr-ref (first (filter #(= (% :node/title) attr-title) refs))
            operation [(get-operator :=)
                       [(first (filter #(not= % attr-ref) refs))
                        ref-type]]]
        (execute-roam-attr-query attr-ref [operation])))))

(defn- execute-block-datomic-query [ref datomic-attr]
  (rd/q '[:find [?block ...]
          :in $ ?ref ?datomic-attr
          :where
          [?ref ?datomic-attr ?block]]
        ref datomic-attr))

(defn- ref-datomic-attr-query? [block]
  (let [block-string (str/trim (block :block/string))
        split-str (str/split block-string #" ")]
    (and (>= 1 (count (block :block/refs)))
         (boolean (some #{(keyword (subs (last split-str) 1))} block-datomic-attrs)))))

(defn- find-longest-ref [refs]
  (reduce #(cond (= %1 nil) %1
                 (> (count (%2 :node/title)) (count (%1 :node/title))) %2
                 :else %1) nil refs))

(defn- ref-datomic-query [block]
  ; A hack to ensure we retrieve [[Test [[A]]]] instead of [[A]]
  (let [ref (find-longest-ref (block :block/refs))
        datomic-attr (extract-datomic-attr (block :block/string))]
    (execute-block-datomic-query ref datomic-attr)))

(defn- roam-attr-query? [block-string]
  (str/includes? block-string "::"))

(defn attr-query? [block]
  (let [block-string (str/trim (block :block/string))]

    (or (roam-attr-query? block-string)
        (datomic-attr-query? block-string)
        (ref-datomic-attr-query? block))))

(defn attr-query [block children]
  (let [block-string (str/trim (block :block/string))]
    (cond (roam-attr-query? block-string) (roam-attr-query block children)
          (datomic-attr-query? block-string) (datomic-attr-query block children)
          (ref-datomic-attr-query? block) (ref-datomic-query block))))

(def m-attr-query
  (memoize attr-query))
