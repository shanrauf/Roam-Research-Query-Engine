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
            [query.util :refer [ref->ref-content]]))

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
          [(?parse-attribute-value ?v ?attribute ?refs) ?v]))])

;; (defn custom-aggregate [values]
;;   (print values)
;;   values)

(defn execute-roam-attr-query [attribute operations]
  (let [operations-pred (operations->datalog-pred operations)]
    (rd/q '[:find [?block ...]
            :in $ ?rules ?attribute ?rdq ?parse-attribute-value ?operations-pred
            :where
            [?block :attrs/lookup ?attribute]
            [(?rdq [:find [?v ...]
                    :in $ % ?block ?attribute ?parse-attribute-value
                    :where
                    (attr-values ?block ?attribute ?parse-attribute-value ?v)]
                   ?rules ?block ?attribute ?parse-attribute-value)
             ?values]
            [(?operations-pred ?values)]]
          [attr-values] attribute rd/q parse-attribute-value operations-pred)))

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

(defn execute-reverse-roam-attr-query [attr-ref input-ref]
  (->> (rd/q '[:find [?v ...]
               :in $ ?attr-ref ?input-ref ?parse-attribute-value %
               :where
               (attr-values ?input-ref ?attr-ref ?parse-attribute-value ?v)]
             attr-ref input-ref parse-attribute-value [attr-values])
       (reduce #(if (= (attr-value->type %2) ref-type)
                  (conj %1 (attr-value->value %2))
                  %1)
               [])))

(defn- reverse-roam-attr-query [block]
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
    (execute-reverse-roam-attr-query attr-ref input-ref)))

(defn- roam-attr-query? [block-string]
  (str/includes? block-string "::"))

(defn- reverse-roam-attr-query? [block-string]
  (str/starts-with? (str/trim block-string) ":"))

(defn attr-query? [block]
  (let [block-string (str/trim (block :block/string))]

    (or (roam-attr-query? block-string)
        (ref-datomic-attr-query? block))))

(defn attr-query [block children]
  (let [block-string (str/trim (block :block/string))]
    (cond (roam-attr-query? block-string) (roam-attr-query block children)
          (reverse-roam-attr-query? block-string) (reverse-roam-attr-query block)
          (ref-datomic-attr-query? block) (ref-datomic-query block))))

(def m-attr-query
  (memoize attr-query))
