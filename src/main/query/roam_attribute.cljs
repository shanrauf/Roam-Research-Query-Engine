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


(defn test-func []
  (println (rd/q '[:find ?attribute ?v
                   :in $ ?test-block-uid
                   :where
                   [?test-block :block/uid ?test-block-uid]
                   [?test-block :attrs/lookup ?l]
                   [?test-block :attrs/lookup ?attribute]
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
                             [(subs ?v 1) ?v])))] "1i_1fbVWa")))

;; (defn all-attributes-and-their-values-for-test-block []
;;   (println (rd/q '[:find ?attribute ?v
;;                    :in $ ?test-block-uid
;;                    :where
;;                    [?test-block :block/uid ?test-block-uid]
;;                    [?test-block :attrs/lookup ?l]
;;                    [?test-block :attrs/lookup ?attribute]
;;                    [?attribute :node/title ?attr-title]
;;                    [?l :block/string ?str]
;;                    [(str ?attr-title "::") ?roam-attr]
;;                    [(clojure.string/starts-with? ?str ?roam-attr)]
;;                     ;; Variables for parsing one-liner attributes w/o clojure.string
;;                    [(count ?attr-title) ?attr-title-len]
;;                    [(+ ?attr-title-len 2) ?roam-attr-len-no-space]

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

;;                          ; "Attr:: val" -> " val" | "Attr::val" -> "val"
;;                          [(subs ?v ?roam-attr-len-no-space) ?v]
;;                          [(subs ?v 0 1) ?first-char]
;;                          (or [(!= ?first-char " ")]
;;                              [(subs ?v 1) ?v])))] "1i_1fbVWa")))

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