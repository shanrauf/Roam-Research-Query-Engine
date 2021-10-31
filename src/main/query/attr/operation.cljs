(ns query.attr.operation
  (:require [clojure.string :as str]
            [query.attr.value :refer [ref-type
                                      num-type
                                      text-type
                                      parse-attribute-value
                                      attr-value->timestamp
                                      attr-value->value
                                      attr-value->type]]))

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

(defn resolve-operation [attr-ref operation]
  (let [operator (-> (first (filter (comp #{0} :block/order) operation))
                     (:block/string)
                     (str/trim)
                     (get-operator))
        input-block (first (filter (comp #{1} :block/order) operator))
        input-str (str/trim (:block/string input-block))
        input-values (mapv #(parse-attribute-value input-str attr-ref %)
                           (:block/refs input-block))]
    [operator input-values]))


(defn- values-pass-operation? [attr-values operation]
  (let [[operator input-values] operation]
    (try (operator attr-values input-values)
         (catch :default e (do (println e)
                               false)))))

(defn passes-operations? [attr-values operations]
  (every? #(values-pass-operation? attr-values %) operations))


(defn operations->datalog-pred [operations]
  (fn [values] (if (> (count operations) 0)
                 (passes-operations? values operations)
                 true)))