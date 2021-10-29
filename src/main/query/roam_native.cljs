(ns query.roam-native
  (:require [clojure.string :as str]
            [cljs.reader :refer [read-string]]
            [query.util :refer [branch-clauses string->md5-hex]]
            [query.errors :refer [throw-error roam-native-error]]))

(defonce roam-native-rule
  '[(roam-native ?block ?refs)
    ; Double negation (StackOverflow)
    (not-join [?block ?refs]
              [(identity ?refs) [?ref ...]]
              (not-join [?block ?ref]
                        (or-join [?block ?ref]
                                 [?block :block/refs ?ref]
                                 (and [?block :block/parents ?parents]
                                      [?parents :block/refs ?ref])
                                 [?block :block/page ?ref])))])

(defn roam-native-query? [block-string]
  ; Replace `` if surrounding the block
  (let [query-str (if (and (str/starts-with? block-string "`")
                           (str/ends-with? block-string "`"))
                    (subs block-string 1 (- (count block-string) 1))
                    block-string)]
    (and (str/starts-with? query-str "{{")
         (str/ends-with? query-str "}}")
         (str/includes? query-str "query"))))

(defn- trim-roam-native-query [block-string]
  (let [query-str (str/trim (subs block-string 2 (- (count block-string) 2)))]
    (if (str/starts-with? query-str "[[")
      (str/trim (subs query-str 10))
      (str/trim (subs query-str 8)))))

; When you find "[[", then keep going until you hit 0 count, then return how long the ref is.
(defn ref-length
  ([expr] (ref-length (rest (str/split expr #"")) 0 0))
  ([[x & xs] count len]
   (cond
     (neg? count) [false len]
     (nil? x) [(zero? count) len]
     ; You hit the end of a page reference
     (and (not (= len 0)) (zero? count)) [true len]
     (= x "[") (recur xs (inc count) (inc len))
     (= x "]") (recur xs (dec count) (inc len))
     :else (recur xs count (inc len)))))

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

(defn- parse-roam-dnp-ref [title]
  (-> (subs title 2 (- (count title) 2))
      (str/replace "," "")
      (str/replace "nd" "")
      (str/replace "th" "")
      (str/replace "st" "")
      (str/replace "rd" "")
      (str/split " ")
      (format-date)))


(defn- date->datalog [date]
  (let [uid (str/replace (.toLocaleDateString date "en-US") "/" "-")]
    [(list 'and
           ['?e :block/uid uid]
           '(roam-native ?block ?e))]))

(defn date-range->datalog [date end-date clauses]
  (if (> date end-date)
    clauses
    (date-range->datalog (js/Date. (.setDate (js/Date. date) (+ 1 (.getDate (js/Date. date)))))
                         end-date
                         (into clauses (date->datalog date)))))

(defn- resolve-between-clause [[date1 date2]]
  (let [d1 (js/Date. (parse-roam-dnp-ref date1))
        d2 (js/Date. (parse-roam-dnp-ref date2))
        startDate (if (< d1 d2)
                    d1
                    d2)
        endDate (if (> d1 d2)
                  d1
                  d2)]
    [(concat (list 'or) (date-range->datalog startDate endDate []))]))

(defn- replace-braces-with-brackets [query-str]
  (-> query-str
      (str/replace "{" "[")
      (str/replace "}" "]")))

(defn- branch-clause->keyword [query-str]
  (reduce #(str/replace %1 (str %2 ":") (str ":" %2)) query-str branch-clauses))

(defn- has-page-ref? [str]
  (str/includes? str "[["))

(defn- str-insert
  "Insert c in string s at index i."
  [s c i]
  (str (subs s 0 i) c (subs s i)))

(defn- refs->strings [query-str start]
  (let [ref-start (+ start (str/index-of (subs query-str start) "[["))
        substr (subs query-str ref-start)
        [is-balanced len] (ref-length substr)]
    (cond (not (has-page-ref? substr)) query-str
          is-balanced (refs->strings (-> query-str
                                         (str-insert "\"" (+ ref-start len))
                                         (str-insert "\"" ref-start))
                                     (-> ref-start
                                         (+ len)
                                         ; Account for the quotes you added
                                         (+ 2)))
          :else (throw-error roam-native-error query-str))))

; TODO Breaks if a page title has a {curly brace} or unbalanced [brackets]
;; You can easily fix by consulting :block/refs & pattern matching with each entry's :block/string
;; but it's a rare edge case and not important rn
(defn parse-query
  "Turn a query string into a list format that read-string can understand.
   
   e.g. {and: [[ex-A]] [[ex-B]]} -> ['and: '[[ex-A]]' '[[ex-B]]']
   "
  [query-str]
  (-> query-str
      (replace-braces-with-brackets)
      (branch-clause->keyword)
      (refs->strings 0)
      #_:clj-kondo/ignore
      (read-string)))

(defn- filter-query-blocks [query]
  (into query '[[?query-ref :node/title "query"]
                (not [?block :block/refs ?query-ref])]))

(defn- nested-clause? [clause]
  (keyword? (nth clause 0)))

(defn- wrap-query-in-branch
  [query current-branch clause-branch-type]
  (cond (= current-branch clause-branch-type) query
        (= clause-branch-type :and) (if (= current-branch :or)
                                      (concat (list 'and) query)
                                      query)
        :else (concat (list (symbol clause-branch-type)) query)))

(defn resolve-roam-native-query [query current-branch-type]
  (let [clause-branch-type (nth query 0)
        query-content (subvec query 1)]
    (if (= clause-branch-type :between)
      (resolve-between-clause query-content)
      (let [child-clauses (mapv #(if (nested-clause? %)
                                   (resolve-roam-native-query % clause-branch-type)
                                   (let
                                    [ref (str %)
                                     ; A little bird suggested I hash the refs (instead of random var names) for testing and (maybe?) caching
                                     free-var (->> (subs ref 2 (- (count ref) 2))
                                                   (string->md5-hex)
                                                   (str "?")
                                                   (symbol))]
                                     [(list 'ref-to-eid ref free-var)
                                      (list 'roam-native '?block free-var)]))
                                query-content)]
        (wrap-query-in-branch (reduce #(into %1 %2) [] child-clauses)
                              current-branch-type
                              clause-branch-type)))))

(defn- roam-native-query [block-string]
  (try
    (-> block-string
        (trim-roam-native-query)
        (parse-query)
        (resolve-roam-native-query :and)
        (filter-query-blocks))
    (catch :default e (println e))))

(def m-roam-native-query
  (memoize roam-native-query))
