(ns query.attr.operation
  (:require [clojure.string :as str]
            [query.util :refer [remove-backticks is_dnp]]
            [query.attr.value :refer [ref-type
                                      num-type
                                      text-type
                                      extract-attr-values
                                      attr-value->timestamp
                                      attr-value->value
                                      attr-value->type]]))
(defn- equals?
  "Check equality (complex procedure because we account for
   duplicates and values of different types)"
  [attr-values input-attr-values]
  (let [val-count (count attr-values)]
    ; Auto-fail if the # of values you're checking equality against aren't the same
    (and (= val-count (count input-attr-values))
         (let [grouped-attr-values (group-by attr-value->type attr-values)
               grouped-input-attr-values (group-by attr-value->type input-attr-values)
               attr-val-keys (set (keys grouped-attr-values))
               input-val-keys (set (keys grouped-input-attr-values))]
           ; Stop if the attr values don't have the same types of values as
           ; input values because then the two lists of values aren't equal
           ; e.g. if attr-values are all text-type and input-values are all
           ; ref-type, then stop because the values are obviously not equal
           (if (= attr-val-keys input-val-keys)
             (every? (fn [val-type]
                       (let [vals (sort (get grouped-attr-values val-type))
                             input-vals (sort (get grouped-input-attr-values val-type))
                             vals-count (count vals)]
                         ; Auto-fail if the # of values of this specific type aren't equal
                         (if (= vals-count (count input-vals))
                           (every? #(= (nth vals %)
                                       (nth input-vals %)) (range 0 vals-count))
                           false))) input-val-keys)
             false)))))

(defn- less-than? [attr-values [input-attr-value]]
  (if (= (attr-value->type input-attr-value) ref-type)
    (let [input-date (attr-value->timestamp input-attr-value)
          date-values (mapv attr-value->timestamp attr-values)]
      (every? #(< % input-date) date-values))
    (every? #(let [input-val (attr-value->value input-attr-value)]
               (< % input-val))
            (mapv attr-value->value attr-values))))

(defn- greater-than? [attr-values [input-attr-value]]
  (if (= (attr-value->type input-attr-value) ref-type)
    (let [input-date (attr-value->timestamp input-attr-value)
          date-values (mapv attr-value->timestamp attr-values)]
      (every? #(> % input-date) date-values))
    (every? #(let [input-val (attr-value->value input-attr-value)]
               (> % input-val))
            (mapv attr-value->value attr-values))))

(defn- less-than-or-equal? [attr-values [input-attr-value]]
  (if (= (attr-value->type input-attr-value) ref-type)
    (let [input-date (attr-value->timestamp input-attr-value)
          date-values (mapv attr-value->timestamp attr-values)]
      (every? #(<= % input-date) date-values))
    (every? #(let [input-val (attr-value->value input-attr-value)]
               (<= % input-val))
            (mapv attr-value->value attr-values))))

(defn- greater-than-or-equal? [attr-values [input-attr-value]]
  (if (= (attr-value->type input-attr-value) ref-type)
    (let [input-date (attr-value->timestamp input-attr-value)
          date-values (mapv attr-value->timestamp attr-values)]
      (every? #(>= % input-date) date-values))
    (every? #(let [input-val (attr-value->value input-attr-value)]
               (>= % input-val))
            (mapv attr-value->value attr-values))))

(defn- is-dnp? [values _]
  (not (boolean (some js/isNaN (mapv attr-value->timestamp values)))))

;; (defn- some-attr-values-satisfy-generic-clause-op [clause-block]
;;   ; make sure all attr values are refs? or wrap i ntry catch if faster? idk
;;   (fn [attr-values _]
;;     (> (count (eval-generic-roam-query (get-query-uid clause-block)
;;                                        (mapv attr-value->value attr-values)))
;;        0)))

;; (defn- all-attr-values-satisfy-generic-clause-op [clause-block]
;;   (fn [attr-values _]
;;     (= (count attr-values)
;;        (count (eval-generic-roam-query (get-query-uid clause-block)
;;                                        (mapv attr-value->value attr-values))))))

(defn- input-regex? [input]
  (and (str/starts-with? input "/")
       (str/ends-with? input "/")))

(defn- input-regex-str->rexp [input]
  (-> input
      (subs 1 (- (count input) 1))
      (str/lower-case)
      (re-pattern)))

; NOTE: We assume there is only one input-value if text-type or num-type because
; we don't support passing in a list of strings/numbers. This may change though...
(defn- includes? [attr-values input-attr-values]
  (let [values (mapv attr-value->value attr-values)
        first-input (first input-attr-values)
        input-type (attr-value->type first-input)
        input-value (attr-value->value first-input)]
    (cond (= input-type text-type)
          (boolean (some #(boolean (if (input-regex? input-value)
                                     (re-find (input-regex-str->rexp input-value)
                                              (str/lower-case %))
                                     (str/includes? % input-value))) values))

          (= input-type num-type)
          (boolean (some #(= input-value %) values))

          :else (every? #(boolean (some #{%}
                                        values)) (map attr-value->value input-attr-values)))))

; The fact that this function even executes means the attribute is not empty
(defn- not-empty? []
  true)

(defn equality-operation? [operation]
  (let [op (first operation)]
    (or (= op equals?)
        (= op includes?))))

(defonce one-line-query-operators
  {is_dnp is-dnp?
   :not_empty not-empty?})

(defonce query-operators
  (merge one-line-query-operators
         {:= equals?
          :!= (fn [values input-values]
                (not (equals? values input-values)))
          ; NOTE: I can't define greater-than? as (not less-than?) because
          ; I sometimes have to short-circuit false if the attribute values
          ; aren't dates and the inverse approach will turn that false into a true
          :< less-than?
          :> greater-than?
          :<= less-than-or-equal?
          :>= greater-than-or-equal?
          :includes includes?
          :contains includes?}))

(defn get-operator [op-name]
  (if (keyword? op-name)
    (get query-operators op-name)
    (get query-operators (keyword (str/lower-case op-name)))))

(defn resolve-operation [attr-ref op-blocks]
  (let [operator-str (-> (first (filter (comp #{0} :block/order) op-blocks))
                         (:block/string)
                         (str/trim)
                         (str/lower-case)
                         (remove-backticks))
        operator (get-operator operator-str)]
    (if (contains? one-line-query-operators (keyword operator-str))
      [operator []]
      (let [input-block (first (filter (comp #{1} :block/order) op-blocks))
            input-str (str/trim (:block/string input-block))]
        (if
        ;;  (and (or (= operator includes?)
        ;;              (= operator equals?))
        ;;          (generic-query-clause? input-block))
        ;;   [(if (= operator includes?)
        ;;      (some-attr-values-satisfy-generic-clause-op input-block)
        ;;      (all-attr-values-satisfy-generic-clause-op input-block))
        ;;    []]
         false
          []
          [operator (if (contains? one-line-query-operators (keyword operator-str))
                      []
                      (extract-attr-values input-str
                                           attr-ref
                                           (mapv :db/id (:block/refs input-block))))])))))


(defn- values-pass-operation? [attr-values operation]
  (let [[operator input-values] operation]
    (operator attr-values input-values)))

(defn passes-operations? [attr-values operations]
  (every? #(values-pass-operation? attr-values %) operations))


(defn operations->-predicate [operations]
  (fn [values] (if (> (count operations) 0)
                 (passes-operations? values operations)
                 true)))