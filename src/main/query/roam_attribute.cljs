(ns query.roam-attribute
  (:require [roam.datascript :as rd]
            [clojure.string :as str]
            [query.util :refer [dnp-title->date-str]]))

(defonce text-type :text-type)
(defonce num-type :num-type)
(defonce ref-type :ref-type)
(defonce roam-ref-regex #"\(\(([^()]+)\)\)|\[\[([^\[\]]+)\]\]")
(defonce float-regex #"^\d+(\.\d+)?$")

(defn- eid->title [eid]
  (ffirst (rd/q '[:find ?title
                  :in $ ?e
                  :where
                  [?e :node/title ?title]]
                eid)))

(defn- attr-value->type [pair]
  (last pair))
(defn- attr-value->value [pair]
  (first pair))

(defn- attr-value->timestamp [attr-val]
  (->> (attr-value->value attr-val)
       (eid->title)
       (dnp-title->date-str)
       (. js/Date parse)))

(defn parse-attribute-ref-value
  "The attribute is a ref type if the value ONLY contains references"
  [original-value value ref]
  (if (re-find roam-ref-regex value)
    (parse-attribute-ref-value original-value
                               (-> (str/replace value roam-ref-regex                                                               "")
                                   (str/trim))
                               ref)
    (if (= "" value)
      [ref ref-type]
      [original-value text-type])))

(defn parse-attribute-value [input attr-ref ref]
  (let [value (str/trim input)]
    (cond
      (re-find float-regex value) [(js/parseFloat value)
                                   num-type]
      (re-find roam-ref-regex value) (if (= attr-ref ref)
                                       nil
                                       (parse-attribute-ref-value value
                                                                  value
                                                                  ref))
      :else [value text-type])))

(defn- equals?
  "Check equality (including duplicates)"
  [values input-attr-values]
  (let [val-count (count values)]
    (and (= val-count (count input-attr-values))
         (let [sorted-values (sort (mapv attr-value->value values))
               sorted-inputs (sort (mapv attr-value->value input-attr-values))]
           (every? #(= (nth sorted-values %)
                       (nth sorted-inputs %)) (range 0 val-count))))))

(defn- less-than? [values [input-attr-value]]
  (if (= (attr-value->type input-attr-value) ref-type)
    (let [input-date (attr-value->timestamp input-attr-value)
          date-values (mapv attr-value->timestamp values)]
      (every? #(< % input-date) date-values))
    (every? #(< % (attr-value->value input-attr-value))
            (mapv attr-value->value values))))

(defn- less-than-or-equal? [values [input-attr-value]]
  (if (= (attr-value->type input-attr-value) ref-type)
    (let [input-date (attr-value->timestamp input-attr-value)
          date-values (mapv attr-value->timestamp values)]
      (every? #(<= % input-date) date-values))
    (every? #(<= % (attr-value->value input-attr-value)) (mapv attr-value->value values))))

(defn- is-dnp? [values _]
  (not (boolean (some js/isNaN (mapv attr-value->timestamp values)))))

(defn- includes? [attr-values input-values]
  (let [values (mapv attr-value->value attr-values)
        first-input (first input-values)
        input-type (attr-value->type first-input)
        input-value (attr-value->value first-input)
        text-input-regex (->> input-value
                              (str)
                              (str/lower-case)
                              (re-pattern))]
    (cond (= input-type text-type)
          (boolean (some #(boolean (re-find text-input-regex
                                            (str/lower-case %))) values))

          (= input-type num-type)
          (boolean (some #(= input-value %) values))

          :else (every? #(boolean (some #{(attr-value->value %)}
                                        values)) input-values))))

(defonce query-operators
  {:= equals?
   :!= (fn [values input-values]
         (not (equals? values input-values)))
   :< less-than?
   :> (fn [values input-values]
        (not (less-than-or-equal? values input-values)))
   :<= less-than-or-equal?
   :>= (fn [values input-values]
         (not (less-than? values input-values)))
   :is_dnp is-dnp?
   :includes includes?
   :contains includes?})

(defn get-operator [op-name]
  (if (keyword? op-name)
    (get query-operators op-name)
    (get query-operators (keyword (str/lower-case op-name)))))

(defn- resolve-operation [attr-ref operation]
  (let [operator (-> (first (filter (comp #{0} :block/order) operation))
                     (:block/string)
                     (str/trim)
                     (get-operator))
        input-block (first (filter (comp #{1} :block/order) operator))
        input-str (str/trim (:block/string input-block))
        input-values (mapv #(parse-attribute-value input-str attr-ref %)
                           (:block/refs input-block))]
    [operator input-values]))


(defn values-pass-operation? [attr-values operation]
  (let [[operator input-values] operation]
    (try (operator attr-values input-values)
         (catch :default e (do (println e)
                               false)))))

(defn passes-operations? [attr-values operations]
  (every? #(values-pass-operation? attr-values %) operations))

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

(defn- operations->datalog-pred [operations]
  (fn [values] (if (> (count operations) 0)
                 (passes-operations? values operations)
                 true)))

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
