(ns query.roam-attribute
  (:require [roam.datascript :as rd]
            [clojure.string :as str]
            [query.util :refer [parse-roam-dnp-ref]]))

(defonce text-type :text-type)
(defonce num-type :num-type)
(defonce ref-type :ref-type)
(defonce roam-ref-regex #"\(\(([^()]+)\)\)|\[\[([^\[\]]+)\]\]")
(defonce float-regex #"^\d+(\.\d+)?$")

(defn parse-attribute-ref-value
  "The attribute is a ref type if the value ONLY contains references"
  [original-value value ref]
  (if (re-find roam-ref-regex value)
    (parse-attribute-ref-value original-value (str/trim (str/replace value roam-ref-regex "")) ref)
    (if (= "" value)
      [ref ref-type]
      [original-value text-type])))

(defn parse-attribute-value [input attr-ref ref]
  ; Filter out the attribute ref
  ;; TODO could cause edge cases if someone's attr value ACTUALLY IS the attribute,
  ;;; but realistically, attributes should prob be a "special class" of refs anyway, not just
  ;;; used as values... Well and Roam should make it easier query attr values if they
  ;;;; are already going through the trouble to set :attrs/lookup and :entity/attrs XD
  (if (= attr-ref ref)
    nil
    (let [value (str/trim input)]
      (cond
        (re-find float-regex value) [(js/parseFloat value)
                                     num-type]
        (re-find roam-ref-regex value) (parse-attribute-ref-value value value ref)
        :else [value text-type]))))

; Check equality (duplicates matter)
(defn- equals? [values input-values]
  (let [val-count (count values)]
    (and (= val-count (count input-values))
         (let [sorted-values (sort (map #(nth % 0) values))
               sorted-inputs (sort (map #(nth % 0) input-values))]
           (every? #(= (nth sorted-values %) (nth sorted-inputs %)) (range 0 val-count))))))

(defn- not-equal? [values input-values]
  (not (equals? values input-values)))

(defn- less-than? [values [input-value]]
  (every? #(< % input-value) (map #(nth % 0) values)))
(defn- less-than-or-equal? [values [input-value]]
  (every? #(<= % input-value) (map #(nth % 0) values)))
(defn- greater-than? [values input-values]
  (not (less-than-or-equal? values input-values)))
(defn- greater-than-or-equal? [values input-values]
  (not (less-than? values input-values)))

(defn- is-dnp? [values _]
  (every? #(js/isNaN (.. js/Date -parse %)) (map parse-roam-dnp-ref (map #(nth % 0) values))))

(defn- includes? [attr-values input-values]
  (let [values (mapv #(nth % 0) attr-values)
        first-input-val (nth input-values 0)]
    (if (= (nth first-input-val 1) text-type)
      (boolean (some #(re-matches (re-pattern (nth first-input-val 0)) %) values))
      (every? #(boolean (some #{(nth % 0)} values)) input-values))))

(defonce operators
  {:= equals?
   :!= not-equal?
   :< less-than?
   :> greater-than?
   :<= less-than-or-equal?
   :>= greater-than-or-equal?
   :IS_DNP is-dnp?
   :INCLUDES includes?
   :CONTAINS includes?})

(defn- get-operator [op-name]
  (if (keyword? op-name)
    (get operators op-name)
    (get operators (keyword op-name))))

(defn- resolve-operator-value-pair [attr-ref op-value-pair]
  (let [operator (-> ((first (filter (comp #{0} :block/order) op-value-pair)))
                     (:block/string)
                     (str/trim)
                     (get-operator))
        input-block (first (filter (comp #{1} :block/order) op-value-pair))
        input-str (str/trim (:block/string input-block))
        input-values (map #(parse-attribute-value input-str attr-ref %) (:block/refs input-block))]
    [operator input-values]))


(defn values-pass-operator? [values op-value-pair]
  (let [[operator input-values] op-value-pair]
    (try (operator values input-values)
         (catch :default _ false))))

(defn passes-operators? [values op-value-pairs]
  (every? #(values-pass-operator? values %) op-value-pairs))

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

; :block/refs
(defn- datomic-attr-query? [block-string]
  (let [split-str (str/split block-string #" ")]
    (and (= 1 (count split-str))
         (boolean (some #{(keyword (subs (first split-str) 1))} block-datomic-attrs)))))

(defn- operator-datalog-predicate [operator-value-pairs]
  (fn [values] (if (> (count operator-value-pairs) 0)
                 (passes-operators? values operator-value-pairs)
                 true)))

(defn datomic-attr-query [datomic-attr operator-value-pair]
  (let [operator-pred (operator-datalog-predicate operator-value-pair)]
    (rd/q '[:find [?block ...]
            :in $ ?datomic-attr ?operator-pred
            :where
            [(?rdq [:find [?v ...]
                    :in $ ?block ?datomic-attr
                    :where
                    [?block ?datomic-attr ?v]] ?block ?datomic-attr) ?attr-values]
            [(operator-pred ?attr-values)]]
          datomic-attr operator-pred)))

(defn- resolve-datomic-attr [block children]
  (let [datomic-attr (keyword (subs (first (str/split (block :block/string) #" ")) 1))
        operator-value-pair (resolve-operator-value-pair datomic-attr children)]
    (datomic-attr-query datomic-attr operator-value-pair)))

(defn- roam-attr-query [attribute operator-value-pairs]
  (let [operator-pred (operator-datalog-predicate operator-value-pairs)]
    (rd/q '[:find [?block ...]
            :in $ ?attribute ?operator-value-pairs ?rdq ?parse-attribute-value ?operator-pred
            :where
            [?block :attrs/lookup ?attribute]
            [(?rdq [:find [?v ...]
                    :in $ ?block ?attribute ?parse-attribute-value
                    :where
                    [?block :attrs/lookup ?l]
                    (or-join
                     [?l ?attribute ?v ?parse-attribute-value]
                     (and [?l :block/parents ?parent]
                          [?parent :block/refs ?attribute]
                          [?parent :block/children ?l]
                          [?l :block/string ?v]
                          (not [(re-matches #"^\s*$" ?v)])
                          [?l :block/refs ?refs]
                          [(?parse-attribute-value ?v ?attribute ?refs) ?v])
                     (and [?l :block/refs ?attribute]
                          [?l :block/refs ?refs]
                          [?l :block/string ?v]

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
                          [(?parse-attribute-value ?v ?attribute ?refs) ?v]))] ?block ?attribute ?parse-attribute-value) ?values]
            [(?operator-pred ?values)]]
          attribute operator-value-pairs rd/q parse-attribute-value operator-pred)))

(defn- resolve-roam-attr [block children]
  ; "::" and no children means = 
  ;; with children means operator
  (let [block-string (block :block/string)
        block-refs (block :block/refs)]
    (if (block :block/children)
      (let [attr-ref (first block-refs)
            operator-value-pair (resolve-operator-value-pair attr-ref children)]
        (roam-attr-query attr-ref operator-value-pair))
      ; TODO: fragile parsing for attr title
      (let [attr-title (first (str/split block-string #"::"))
            attr-ref (first (filter #(= (% :node/title) attr-title) block-refs))
            operator-value-pair [(get-operator :=) [(first (filter #(not= % attr-ref) block-refs)) ref-type]]]
        (roam-attr-query attr-ref operator-value-pair)))))

(defn- block-datomic-query [ref datomic-attr]
  (rd/q '[:find [?block ...]
          :in $ ?ref ?datomic-attr
          :where
          [?ref ?datomic-attr ?block]]
        ref datomic-attr))

; [[Ref [[Maybe Nested]]]] :block/children
(defn- ref-datomic-attr-query? [block]
  (let [block-string (str/trim (block :block/string))
        split-str (str/split block-string #" ")]
    (and (>= 1 (count (block :block/refs)))
         (boolean (some #{(keyword (subs (last split-str) 1))} block-datomic-attrs)))))

(defn- find-longest-ref [refs]
  (reduce #(cond (= %1 nil) %1
                 (> (count (%2 :node/title)) (count (%1 :node/title))) %2
                 :else %1) nil refs))

(defn- resolve-ref-datomic-attr-query [block]
  ; We can assume the ref we want has the longest title
  (let [ref (find-longest-ref (block :block/refs))
        datomic-attr (keyword (subs (last (str/split (block :block/string) #" ")) 1))]
    (block-datomic-query ref datomic-attr)))

; TODO "::"" check is very simplistic
; Attr::
(defn- roam-attr-query? [block-string]
  (str/includes? block-string "::"))

(defn attr-query? [block]
  (let [block-string (str/trim (block :block/string))]

    (or (roam-attr-query? block-string)
        (datomic-attr-query? block-string)
        (ref-datomic-attr-query? block))))

(defn attr-query [block children]
  (let [block-string (str/trim (block :block/string))]
    (cond (roam-attr-query? block-string) (resolve-roam-attr block children)
          (datomic-attr-query? block-string) (resolve-datomic-attr block children)
          (ref-datomic-attr-query? block) (resolve-ref-datomic-attr-query block))))

(defn test-func []
  (println (roam-attr-query 63 [[(get-operator :INCLUDES) [[188 ref-type]]]])))

(def m-attr-query
  (memoize attr-query))
