(ns query.util
  (:require goog.crypt goog.crypt.Md5
            [clojure.string :as str]))

(defonce branch-clauses ["and" "or" "not" "between"])
(defn branch? [branch]
  (some #(= branch %) branch-clauses))

(defonce months {:January 1
                 :February 2
                 :March 3
                 :April 4
                 :May 5
                 :June 6
                 :July 7
                 :August 8
                 :September 9
                 :October 10
                 :November 11
                 :December 12})

(defn- month-str->month-num [str]
  (months (keyword str)))

(defn- format-date [[month day year]]
  (str (month-str->month-num month) "/" (str day) "/" (str year)))

(defn ref->ref-content [ref]
  (subs ref 2 (- (count ref) 2)))

(defn dnp-title->date-str [title]
  (-> title
      (str/replace "," "")
      (str/replace "nd" "")
      (str/replace "th" "")
      (str/replace "st" "")
      (str/replace "rd" "")
      (str/split " ")
      (format-date)))


(defn wrap-query-in-branch
  [query current-branch clause-branch-type]
  (cond (= current-branch clause-branch-type) query
        (= clause-branch-type :and) (if (= current-branch :or)
                                      (concat (list 'and) query)
                                      query)
        :else (concat (list (symbol clause-branch-type)) query)))

(defn- vec-insert [v idx value]
  (reduce #(into %1 %2) [] [(subvec v 0 idx) [value] (subvec v idx)]))

(defn add-current-blocks-to-query [current-blocks query]
  (let [where-idx (.indexOf query :where)
        new-query (vec-insert query where-idx '?current-blocks)]
    (if (seq current-blocks)
      (vec-insert new-query (+ where-idx 2) '[(identity ?current-blocks) [?block ...]])
      new-query)))

(defn filter-query-blocks [query]
  (into query '[(not [?query-ref :node/title "query"]
                     [?block :block/refs ?query-ref])]))