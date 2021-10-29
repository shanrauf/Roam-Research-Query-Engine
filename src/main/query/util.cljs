(ns query.util)

(defonce branch-clauses ["and" "or" "not" "between"])
(defn branch? [branch]
  (some #(= branch %) branch-clauses))
