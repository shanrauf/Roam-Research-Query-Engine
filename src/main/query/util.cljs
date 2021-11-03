(ns query.util
  (:require [clojure.string :as str]))

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

(defn- vec-insert [v idx value]
  (reduce #(into %1 %2) [] [(subvec v 0 idx) [value] (subvec v idx)]))

(defn- index-of
  "ClojureScript replacement for .indexOf, which won't work in {{roam/render}}"
  [s v]
  (loop [idx 0 items s]
    (cond
      (empty? items) nil
      (= v (first items)) idx
      :else (recur (inc idx) (rest items)))))

(defn add-current-blocks-to-query [current-blocks query]
  (let [where-idx (index-of query :where)
        new-query (vec-insert query where-idx '?current-blocks)]
    (if (seq current-blocks)
      (vec-insert new-query (+ where-idx 2) '[(ground ?current-blocks) [?block ...]])
      new-query)))

(defn filter-query-blocks [where-clauses]
  (into where-clauses '[[?query-ref :node/title "query"]
                        (not-join [?block ?query-ref]
                                  (or-join
                                   [?block ?query-ref]
                                   [?block :block/refs ?query-ref]
                                   (and [?block :block/parents ?parents]
                                        [?parents :block/refs ?query-ref])))]))

(defn remove-backticks [block-string]
  (if (and (str/starts-with? block-string "`")
           (str/ends-with? block-string "`"))
    (subs block-string 1 (- (count block-string) 1))
    block-string))
