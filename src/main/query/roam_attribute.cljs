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
      (every? #(contains? values %) input-values))))

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
  (get operators op-name))

(defn- resolve-operator-value-pair [op-value-pair]
  ; get children with :block/order 0, that's the operator
  (let [operator (-> ((first (filter (comp #{0} :block/order) op-value-pair)))
                     (:block/string)
                     (str/trim)
                     (get-operator))
        ; LEFT OFF
        input-values (-> ((first (filter (comp #{0} :block/order) op-value-pair)))
                         (:block/string)
                         (str/trim)
                         (get-operator))]
    [operator input-values]))


(defn value-passes-operator? [values op-value-pair]
  (let [[operator input-values] op-value-pair]
    (try (operator values input-values)
         (catch :default _ false))))

(defn passes-operators? [values op-value-pairs]
  (every? #(value-passes-operator? values %) op-value-pairs))

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

(defn- datomic-attr? [block-string]
  (let [split-str (str/split block-string #" ")]
    (and (= 1 (count split-str))
         (contains? block-datomic-attrs (keyword (subs (first split-str) 1))))))

(defn- resolve-datomic-attr [block children]
  [block children])

(defn- block-datomic-attr? [block]
  (let [block-string (str/trim (block :block/string))
        split-str (str/split block-string #" ")]
    (and (>= 1 (count (block :block/refs)))
         (contains? block-datomic-attrs (keyword (subs (last split-str) 1))))))

; TODO make sure you get the actual ref, not a nested ref (you'll need to find the longest :node/title for every ref)
(defn- resolve-block-datomic-attr [block]
  (let [ref (block :block/refs)]
    ref))

(defn attr-query? [block]
  (let [block-string (str/trim (block :block/string))]
    ; TODO ":: check is very simlistic"
    ; Attr::
    (or (str/includes? block-string "::")
        ; :block/refs
        (datomic-attr? block-string)
        ; [[Ref [[Maybe Nested]]]] :block/children
        (block-datomic-attr? block))))

(defn attr-query [block children]
  (let [block-string (str/trim (block :block/string))]
    (cond (str/includes? block-string "::")
          ;; "::" and no children means = 
          ;;; with children means operator
          (if (block :block/children)
            (let [[operator input-values] (resolve-operator-value-pair children)])
            false)

          (datomic-attr? block-string) (resolve-datomic-attr block children)

          (block-datomic-attr? block) (resolve-block-datomic-attr block))))

(defn roam-attr-query [attribute operator-value-pairs]
  (let [operator-pred (fn [values] (passes-operators? values operator-value-pairs))]
    (rd/q '[:find ?block ?values
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

(defn test-func []
  (println (roam-attr-query 63 [])))

(def m-attr-query
  (memoize attr-query))