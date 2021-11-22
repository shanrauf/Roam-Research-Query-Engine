(ns query.attr.operation
  (:require [clojure.string :as str]
            [query.util :refer [remove-backticks]]
            [query.attr.value :refer [ref-type
                                      num-type
                                      text-type
                                      parse-attribute-value
                                      attr-value->timestamp
                                      attr-value->value
                                      attr-value->type]]))
(defn- equals?
  "Check equality (complex procedure because we account for
   duplicates and values of different types)"
  [attr-values input-values]
  (let [val-count (count attr-values)]
    ; Auto-fail if the # of values you're checking equality against aren't the same
    (and (= val-count (count input-values))
         ; TODO throws when an attribute's values are different types (cuz u can't compare them)
         (let [grouped-attr-values (group-by attr-value->type attr-values)
               grouped-input-values (group-by attr-value->type input-values)
               attr-val-keys (set (keys grouped-attr-values))
               input-val-keys (set (keys grouped-input-values))]
           ; Stop if the attr values don't have the same types of values as
           ; input values because then the two lists of values aren't equal
           ; e.g. if attr-values are all text-type and input-values are all
           ; ref-type, then stop because the values are obviously not equal
           (if (= attr-val-keys input-val-keys)
             (every? (fn [val-type]
                       (let [vals (sort (get grouped-attr-values val-type))
                             input-vals (sort (get grouped-input-values val-type))
                             vals-count (count vals)]
                         ; Auto-fail if the # of values of this specific type aren't equal
                         (if (= vals-count (count input-vals))
                           (every? #(= (nth vals %)
                                       (nth input-vals %)) (range 0 vals-count))
                           false))) input-val-keys)
             false)))))

(defn- less-than? [attr-values [input-value]]
  (if (= (attr-value->type input-value) ref-type)
    (let [input-date (attr-value->timestamp input-value)
          date-values (mapv attr-value->timestamp attr-values)]
      (every? #(< % input-date) date-values))
    (every? #(< % (attr-value->value input-value))
            (mapv attr-value->value attr-values))))

(defn- less-than-or-equal? [attr-values [input-value]]
  (if (= (attr-value->type input-value) ref-type)
    (let [input-date (attr-value->timestamp input-value)
          date-values (mapv attr-value->timestamp attr-values)]
      (every? #(<= % input-date) date-values))
    (every? #(<= % (attr-value->value input-value)) (mapv attr-value->value attr-values))))

(defn- is-dnp? [values _]
  (not (boolean (some js/isNaN (mapv attr-value->timestamp values)))))

(defn- input-regex? [input]
  (and (str/starts-with? input "/")
       (str/ends-with? input "/")))

(defn- input-regex-str->rexp [input]
  (-> input
      (subs 1 (- (count input) 1))
      (str/lower-case)
      (re-pattern)))

(defn- includes? [attr-values input-values]
  (let [values (mapv attr-value->value attr-values)
        first-input (first input-values)
        input-type (attr-value->type first-input)
        input-value (attr-value->value first-input)]
    ;; (println input-values)
    (cond (= input-type text-type)
          (boolean (some #(boolean (if (input-regex? input-value)
                                     (re-find (input-regex-str->rexp input-value)
                                              (str/lower-case %))
                                     (str/includes? % input-value))) values))

          (= input-type num-type)
          (boolean (some #(= input-value %) values))

          :else (every? #(boolean (some #{%}
                                        values)) (map attr-value->value input-values)))))

(defn equality-operation? [operation]
  (let [op (first operation)]
    (or (= op equals?)
        (= op includes?))))

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
                     (remove-backticks)
                     (get-operator))
        input-block (first (filter (comp #{1} :block/order) operation))
        input-str (str/trim (:block/string input-block))
        input-values (mapv #(parse-attribute-value input-str attr-ref (:db/id %))
                           ; When the input value is a string, number, etc (not a ref)
                           ;; then :block/refs is empty so just have a one-item vec
                           ;;; so that it iterates once
                           ;; TODO Refactor
                           (or (:block/refs input-block) [nil]))]
    [operator input-values]))


(defn- values-pass-operation? [attr-values operation]
  (let [[operator input-values] operation]
    (operator attr-values input-values)))

(defn passes-operations? [attr-values operations]
  (every? #(values-pass-operation? attr-values %) operations))


(defn operations->-predicate [operations]
  (fn [values] (if (> (count operations) 0)
                 (passes-operations? values operations)
                 true)))