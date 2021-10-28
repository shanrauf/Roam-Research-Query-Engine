(ns query.util)

(defn rand-str [len]
  (apply str (take len (repeatedly #(char (+ (rand 26) 65))))))

(defonce branch-clauses ["and" "or" "not" "between"])
(defn branch? [branch]
  (some #(= branch %) branch-clauses))

(defn branch->datalog [current-branch nested-branch child-clauses]
  (cond (= current-branch nested-branch) child-clauses
        (= nested-branch "and") (if (= current-branch "or")
                                  [(apply list (into ['and] child-clauses))]
                                  child-clauses)
        :else [(apply list (into [(symbol nested-branch)] child-clauses))]))