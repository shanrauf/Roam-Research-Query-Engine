(ns query.roam-attribute
  (:require [roam.datascript :as rd]
            [clojure.string :as str]))

;; (defn init []
;;   (println (rd/q '[:find ?blocks ?attr-value-pairs
;;                    :where
;;                    ; So first, find all of the attribute-valur pairs for ?blocks (rn searching whole db)
;;                    ; So subquery that returns ?attribute and [?values ...]
;;                    [(rd/q [:find ?attribute ?values
;;                            :in $ ?block
;;                            :where
;;                            [?block :attrs/lookup ?l]
;;                            [?block :attrs/lookup ?attribute]
;;                            [?attribute :node/title ?attr-title]
;;                            [?l :block/string ?str]
;;                            [(str ?attr-title "::") ?roam-attr]
;;                            [(clojure.string/starts-with? ?str ?roam-attr)]
;;                            ;; Variables for parsing one-liner attributes w/o clojure.string
;;                            [(count ?attr-title) ?attr-title-len]
;;                            [(+ ?attr-title-len 2) ?roam-attr-len-no-space]
;;                            [(rd/q [:find [?v ...]
;;                                    :in $ ?attribute ?roam-attr-len-no-space
;;                                    :where
;;                                    (or-join
;;                                     [?l ?attribute ?v ?roam-attr-len-no-space ?roam-attr ?type ?parse-attr-val]
;;                                     ; Multi-val
;;                                     (and [?l :block/parents ?parent]
;;                                          [?parent :block/refs ?attribute]
;;                                          [?parent :block/children ?l]
;;                                          [?l :block/string ?v]
;;                                          [(!= ?v ?roam-attr)]
;;                                          [?l :block/refs ?refs]
;;                                          [(?parse-attr-val ?v ?refs) [?v ?type]])
;;                                     ; One-liner
;;                                     (and [?l :block/refs ?attribute]
;;                                          [?l :block/refs ?refs]
;;                                          [?l :block/string ?v]

;;                                          ; "Attr:: val" -> " val" | "Attr::val" -> "val"
;;                                          [(subs ?v ?roam-attr-len-no-space) ?v]
;;                                          [(subs ?v 0 1) ?first-char]
;;                                          (or [(!= ?first-char " ")]
;;                                              [(subs ?v 1) ?v])
;;                                          [(?parse-attr-val ?v ?refs) [?v ?type]]))] ?attribute ?roam-attr-len-no-space) ?values]] ?blocks) ?attr-value-pairs]])))

; Note: I manually ref :x-type in queries instead of using these variables, so it's fragile...
(defonce text-type :text-type)
(defonce num-type :num-type)
(defonce ref-type :ref-type)

(defonce roam-ref-regex #"\(\(([^()]+)\)\)|\[\[([^\[\]]+)\]\]")
(defonce whitespace-only-regex #"^\A\s*\z$")
(defonce float-regex #"^\d+(\.\d+)?$")

(defn parse-attribute-ref-value
  "The attribute is a ref type if the value ONLY contains references"
  [original-value value ref]
  (if (re-find roam-ref-regex value)
    (parse-attribute-ref-value original-value (str/trim (str/replace value roam-ref-regex "")) ref)
    ; TODO shouldnt it be: If whitespace only, that means there were only refs and i got rid of em all...?
    (if (= "" value)
      [ref ref-type]
      [original-value text-type])))

; TODO filter out the attr ref from :block/refs so u dont have to do it here...
(defn parse-attribute-value [input attr-ref ref]
  ;; (println input)
  ; Filter out the attribute ref
  ;; TODO could cause edge cases if someone's attr value ACTUALLY IS the attribute,
  ;;; but realistically, attributes should prob be a "special class" of refs anyway, not just
  ;;; used as values... Well and it should be easier to query attr values XD
  (if (= attr-ref ref)
    nil
    (let [value (str/trim input)]
      (cond
        (re-find float-regex value) [(js/parseFloat value)
                                     num-type]
        (re-find roam-ref-regex value) (parse-attribute-ref-value value value ref)
        :else [value text-type]))))

; Infer the type from the value
(defn- val->type-val-pair [v])

(defn- get-operator [op-name])

(defn- resolve-operator [op-value-pair]
  (let [[op-type op-name input-values] op-value-pair]
    [(if (= op-type "every?")
       every?
       some?)
     (get-operator op-name)
     ; TODO but i prob already know the type from parsing the query
     input-values]))

(defn- equals? [values input-values])
  ; if text or numbers, self explanatory
  ; if refs, u check if the eids are equal
  ;; are refs equal to the eids rn? i think rn they're equal to strings... somehow
  ;;; oh right cuz i need to parse the value within the datalog probably so that i can access :block/refs


(defn- includes? [values input-values]
  (every? #(contains? values %) input-values))

; TODO when I resolve the operator function, i should take into account the input value type?
; wait, what if you do like block includes multiple refs? then you dont want to itertae over every value
;; u want to check if the list of values includes every value u care about.
; wait im gonna need custom operator functions for every opreator no matter what, 
(defn value-passes-operator? [values op-value-pair]
  (let [[operator input] (resolve-operator op-value-pair)]
    ;; (if (= operator includes?)
    ;;   (includes? values input)
    ;;   (every? #(-> %
    ;;                (val->type-val-pair)) values))
    (operator values input)))

; so u prob need a custom function for diff classes of operators
; e.g. for =, u want a function that does that properly based on if :ref-type or whatever
;; and for > comparisons, u need ot check if it's a :date-ref-type or a number
(defn passes-operators? [values op-value-pairs]
  (every? #(value-passes-operator? values %) op-value-pairs))

(defn test-func []
  (println (rd/q '[:find ?block ?values
                   :in $ ?rdq ?parse-attribute-value
                   :where
                   ; Status attribute
                   [(identity 63) ?attribute]
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
                                 ; todo do i need this??
                                 ; [(!= ?v ?roam-attr)]
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
                                 [(?parse-attribute-value ?v ?attribute ?refs) ?v]))] ?block ?attribute ?parse-attribute-value) ?values]]
                 rd/q parse-attribute-value)))

;; (defn find-all-values-for-an-attr-on-blocks []
;;   (println (rd/q '[:find ?block ?values
;;                    :in $ ?rdq
;;                    :where
;;                    [(identity 58) ?attribute]
;;                    [?block :attrs/lookup ?attribute]
;;                    [(?rdq [:find [?v ...]
;;                            :in $ ?block ?attribute
;;                            :where
;;                            [?block :attrs/lookup ?l]

;;                            [?attribute :node/title ?attr-title]
;;                            [?l :block/string ?str]
;;                            [(str ?attr-title "::") ?roam-attr]
;;                            [(clojure.string/starts-with? ?str ?roam-attr)]
;;                            [(count ?attr-title) ?attr-title-len]
;;                            [(+ ?attr-title-len 2) ?roam-attr-len-no-space]
;;                            (or-join
;;                             [?l ?attribute ?v ?roam-attr-len-no-space ?roam-attr]
;;                             (and [?l :block/parents ?parent]
;;                                  [?parent :block/refs ?attribute]
;;                                  [?parent :block/children ?l]
;;                                  [?l :block/string ?v]
;;                                  [(!= ?v ?roam-attr)]
;;                                  [?l :block/refs ?refs])
;;                             (and [?l :block/refs ?attribute]
;;                                  [?l :block/refs ?refs]
;;                                  [?l :block/string ?v]

;;                                  [(subs ?v ?roam-attr-len-no-space) ?v]
;;                                  [(subs ?v 0 1) ?first-char]
;;                                  (or [(!= ?first-char " ")]
;;                                      [(subs ?v 1) ?v])))] ?block ?attribute) ?values]]
;;                  rd/q)))

;; (defn test-func []
;;   (println (rd/q '[:find ?block ?attribute ?v
;;                    :in $ %
;;                    :where
;;                    ; Type attr
;;                   ;;  [(identity 58) ?attribute]
;;                    (attr-value-pairs ?block ?attribute ?v)]
;;                  [attr-value-pairs])))

;; (defn functioning-nested-query []
;;   (println (rd/q '[:find ?block ?attr-value-pairs
;;                    :in $ ?rdq
;;                    :where
;;                    (or [?block :block/uid "1i_1fbVWa"]
;;                        [?block :block/uid "w9YC_4vTm"])
;;                    [(?rdq [:find [?attr-value-pair ...]
;;                            :in $ ?block ?rdq
;;                            :where
;;                            [?block :attrs/lookup ?l]
;;                            [?block :attrs/lookup ?attribute]
;;                            [?attribute :node/title ?attr-title]
;;                            [?l :block/string ?str]
;;                            [(str ?attr-title "::") ?roam-attr]
;;                            [(clojure.string/starts-with? ?str ?roam-attr)]
;;                            [(count ?attr-title) ?attr-title-len]
;;                            [(+ ?attr-title-len 2) ?roam-attr-len-no-space]
;;                            [(?rdq [:find [?v ...]
;;                                    :in $ ?l ?attribute ?roam-attr-len-no-space ?roam-attr ?rdq
;;                                    :where
;;                                    (or-join
;;                                     [?l ?attribute ?v ?roam-attr-len-no-space ?roam-attr]
;;                                     (and [?l :block/parents ?parent]
;;                                          [?parent :block/refs ?attribute]
;;                                          [?parent :block/children ?l]
;;                                          [?l :block/string ?v]
;;                                          [(!= ?v ?roam-attr)]
;;                                          [?l :block/refs ?refs])
;;                                     (and [?l :block/refs ?attribute]
;;                                          [?l :block/refs ?refs]
;;                                          [?l :block/string ?v]

;;                                          [(subs ?v ?roam-attr-len-no-space) ?v]
;;                                          [(subs ?v 0 1) ?first-char]
;;                                          (or [(!= ?first-char " ")]
;;                                              [(subs ?v 1) ?v])))] ?l ?attribute ?roam-attr-len-no-space ?roam-attr ?rdq) ?values]
;;                            [(vector ?attribute ?values) ?attr-value-pair]] ?block ?rdq) ?attr-value-pairs]
;;                   ;;  [(identity ?attr-value-pairs) [?attribute ?values]]
;;                   ;;  [?attribute :node/title "Status"]
;;                   ;;  [(= ?values "[[InProgress]]")]
;;                    ]
;;                  rd/q)))

;; (defn working-block-to-attrvalue-pair-query []
;;   (println (rd/q '[:find ?block ?attr-value-pairs
;;                    :in $ ?rdq
;;                    :where
;;                    (or [?block :block/uid "1i_1fbVWa"]
;;                        [?block :block/uid "w9YC_4vTm"])
;;                    [(?rdq [:find [?attr-value-pair ...]
;;                            :in $ ?block ?rdq
;;                            :where
;;                            [?block :attrs/lookup ?l]
;;                            [?block :attrs/lookup ?attribute]
;;                            [?attribute :node/title ?attr-title]
;;                            [?l :block/string ?str]
;;                            [(str ?attr-title "::") ?roam-attr]
;;                            [(clojure.string/starts-with? ?str ?roam-attr)]
;;                            [(count ?attr-title) ?attr-title-len]
;;                            [(+ ?attr-title-len 2) ?roam-attr-len-no-space]
;;                            [(?rdq [:find [?v ...]
;;                                    :in $ ?l ?attribute ?roam-attr-len-no-space ?roam-attr ?rdq
;;                                    :where
;;                                    (or-join
;;                                     [?l ?attribute ?v ?roam-attr-len-no-space ?roam-attr]
;;                                     (and [?l :block/parents ?parent]
;;                                          [?parent :block/refs ?attribute]
;;                                          [?parent :block/children ?l]
;;                                          [?l :block/string ?v]
;;                                          [(!= ?v ?roam-attr)]
;;                                          [?l :block/refs ?refs])
;;                                     (and [?l :block/refs ?attribute]
;;                                          [?l :block/refs ?refs]
;;                                          [?l :block/string ?v]

;;                                          [(subs ?v ?roam-attr-len-no-space) ?v]
;;                                          [(subs ?v 0 1) ?first-char]
;;                                          (or [(!= ?first-char " ")]
;;                                              [(subs ?v 1) ?v])))] ?l ?attribute ?roam-attr-len-no-space ?roam-attr ?rdq) ?values]
;;                            [(vector ?attribute ?values) ?attr-value-pair]] ?block ?rdq) ?attr-value-pairs]]
;;                  rd/q)))

;; (defn test-func []
;;   (println (rd/q '[:find ?blocks ?attribute ?v
;;                    :in $ %
;;                    :where
;;                    (or [?blocks :block/uid "1i_1fbVWa"]
;;                        [?blocks :block/uid "w9YC_4vTm"])
;;                    (attr-value-pairs ?blocks ?attribute ?v)
;;                    ; The problem with this is it filters out all otehr attr values which I rarely want...
;;                    ; hmm, i mean I could look at what attrs they specify in Columns/Include and then do like a ?v not empty thing for those
;;                    ; but idk...
;;                    [?attribute :node/title "Type"]
;;                    [(= ?v "[[Task]]")]] [attribute-value-pairs])))

;; (defn all-attributes-and-their-values-for-test-block []
;;   (println (rd/q '[:find ?attribute ?v
;;                    :in $ ?test-block-uid %
;;                    :where
;;                    [?test-block :block/uid ?test-block-uid]
;;                    (attr-value-pairs ?test-block ?attribute ?v)] "1i_1fbVWa" [attribute-value-pairs])))

;; (defn unique-values-for-attr []
;;   (println (rd/q '[:find [?v ...]
;;                    :in $ ?attribute ?roam-attr-len-no-space ?roam-attr
;;                    :where
;;                   ;;  [?blocks :attrs/lookup ?l]
;;                    (or-join
;;                     [?l ?attribute ?v ?roam-attr-len-no-space ?roam-attr]
;;                     ; Multi-val
;;                     (and [?l :block/parents ?parent]
;;                          [?parent :block/refs ?attribute]
;;                          [?parent :block/children ?l]
;;                          [?l :block/string ?v]
;;                          [(!= ?v ?roam-attr)]
;;                          [?l :block/refs ?refs])
;;                     ; One-liner
;;                     (and [?l :block/refs ?attribute]
;;                          [?l :block/refs ?refs]
;;                          [?l :block/string ?v]

;;                                          ; "Attr:: val" -> " val" | "Attr::val" -> "val"
;;                          [(subs ?v ?roam-attr-len-no-space) ?v]
;;                          [(subs ?v 0 1) ?first-char]
;;                          (or [(!= ?first-char " ")]
;;                              [(subs ?v 1) ?v])))] 58 6 "Type::")))