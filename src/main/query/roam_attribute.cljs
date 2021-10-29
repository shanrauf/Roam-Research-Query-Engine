(ns query.roam-attribute
  (:require [roam.datascript :as rd]))

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


(defonce attr-value-pairs
  '[(attr-value-pairs ?blocks ?attribute ?v)
    [?blocks :attrs/lookup ?l]
    [?blocks :attrs/lookup ?attribute]
    [?attribute :node/title ?attr-title]
    [?l :block/string ?str]
    [(str ?attr-title "::") ?roam-attr]
    [(clojure.string/starts-with? ?str ?roam-attr)]
    ;; Variables for parsing one-liner attributes w/o clojure.string
    [(count ?attr-title) ?attr-title-len]
    [(+ ?attr-title-len 2) ?roam-attr-len-no-space]
    (or-join
     [?l ?attribute ?v ?roam-attr-len-no-space ?roam-attr]
    ; Multi-val
     (and [?l :block/parents ?parent]
          [?parent :block/refs ?attribute]
          [?parent :block/children ?l]
          [?l :block/string ?v]
          [(!= ?v ?roam-attr)]
          [?l :block/refs ?refs])
    ; One-liner
     (and [?l :block/refs ?attribute]
          [?l :block/refs ?refs]
          [?l :block/string ?v]

         ; "Attr:: val" -> " val" | "Attr::val" -> "val"
          [(subs ?v ?roam-attr-len-no-space) ?v]
          [(subs ?v 0 1) ?first-char]
          (or [(!= ?first-char " ")]
              [(subs ?v 1) ?v])))])

(defn test-func []
  (println (rd/q '[:find [?block ...]
                   :in $ %
                   :where
                   ; Type attr
                   [(identity 58) ?attribute]
                   (attr-value-pairs ?block ?attribute ?v)
                   [(= ?v "[[Task]]")]]
                 [attr-value-pairs])))

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