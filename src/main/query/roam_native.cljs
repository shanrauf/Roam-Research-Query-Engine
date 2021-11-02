(ns query.roam-native
  (:require [clojure.string :as str]
            [cljs.reader :refer [read-string]]
            [query.util :refer [ref->ref-content
                                remove-backticks
                                add-current-blocks-to-query
                                branch-clauses
                                dnp-title->date-str
                                filter-query-blocks]]
            [query.errors :refer [throw-error roam-native-error]]
            [roam.datascript :as rd]))

(defonce roam-native-rule
  '[(roam-native ?block ?refs)
    ; Double negation
    (not-join [?block ?refs]
              [(identity ?refs) [?ref ...]]
              (not-join [?block ?ref]
                        (or-join [?block ?ref]
                                 [?block :block/refs ?ref]
                                 [?block :block/parents ?ref]
                                 (and [?block :block/parents ?parents]
                                      [?parents :block/refs ?ref]))))])

(defonce query-rules ['[(ref-to-eid ?ref ?e)
                        [(str ?ref) ?ref-str]
                        [(count ?ref-str) ?len]
                        [(- ?len 2) ?end]
                        [(subs ?ref-str 2 ?end) ?str]
                        (or (and [(clojure.string/starts-with? ?ref-str "[[")]

                                 [?e :node/title ?str])
                            (and [(clojure.string/starts-with? ?ref-str "((")]

                                 [?e :block/uid ?str]))]
                      roam-native-rule])

(defn roam-native-query? [block-string]
  (let [query-str (remove-backticks block-string)]
    (and (str/starts-with? query-str "{{")
         (str/ends-with? query-str "}}")
         (str/includes? query-str "query"))))

(defn- trim-roam-native-query [block-string]
  (let [query-str (remove-backticks block-string)
        query (-> (subs query-str 2 (- (count query-str) 2))
                  (str/trim))]
    (if (str/starts-with? query "[[")
      (str/trim (subs query 10))
      (str/trim (subs query 8)))))

; TODO: breaks with unbalanced brackets e.g. [[Page [ ABC]]
(defn ref-length
  "Walk through a page ref & return its length"
  ([expr] (ref-length (rest (str/split expr #"")) 0 0))
  ([[x & xs] count len]
   (cond
     (neg? count) [false len]
     (nil? x) [(zero? count) len]
     (and (not (= len 0)) (zero? count)) [true len]
     (= x "[") (recur xs (inc count) (inc len))
     (= x "]") (recur xs (dec count) (inc len))
     :else (recur xs count (inc len)))))

(defn- date->datalog [date]
  (let [uid (str/replace (.toLocaleDateString date "en-US") "/" "-")]
    [(list 'and
           ['?e :block/uid uid]
           '[?block :block/refs ?e])]))

(defn date-range->datalog [date end-date clauses]
  (if (> date end-date)
    clauses
    (let [start-date (js/Date. (.setDate (js/Date. date)
                                         (+ 1 (.getDate (js/Date. date)))))]
      (date-range->datalog start-date
                           end-date
                           (into clauses (date->datalog date))))))

(defn- dnp-ref->date-str [ref]
  (-> (ref->ref-content ref)
      (dnp-title->date-str)
      (js/Date.)))

(defn- resolve-between-clause [[date1 date2]]
  (let [d1 (dnp-ref->date-str date1)
        d2 (dnp-ref->date-str date2)
        [start-date end-date] (sort [d1 d2])]
    (date-range->datalog start-date end-date [])))

; TODO: Breaks page titles with curly braces e.g. [[Page {A}]]
(defn- replace-braces-with-brackets [query-str]
  (-> query-str
      (str/replace "{" "[")
      (str/replace "}" "]")))

(defn- branch-clause->keyword [query-str]
  (reduce #(str/replace %1 (str %2 ":") (str ":" %2))
          query-str
          branch-clauses))

(defn- str-insert
  "Insert c in string s at index i."
  [s c i]
  (str (subs s 0 i) c (subs s i)))

(defn- page-refs->strings [query-str start]
  (let [ref-start (+ start (str/index-of (subs query-str start) "[["))
        substr (subs query-str ref-start)
        [is-balanced len] (ref-length substr)]
    (cond (not (str/includes? substr "[[")) query-str
          is-balanced (page-refs->strings (-> query-str
                                              (str-insert "\"" (+ ref-start len))
                                              (str-insert "\"" ref-start))
                                          (-> ref-start
                                              (+ len)
                                              (+ 2)))
          :else (throw-error roam-native-error query-str))))


(defn- nested-clause? [clause]
  (keyword? (nth clause 0)))

(defn- ref->eid [ref]
  (first (rd/q '[:find ?e
                 :in $ % ?ref
                 :where
                 (ref-to-eid ?ref ?e)]
               query-rules ref)))

(defn- combine-lists [lists]
  (reduce #(into %1 %2) [] lists))

(defn- reduce-query
  "Remove duplicates: [:and A [:and B]] -> [:and A B]
   Filter OR NOT: [:or [:and A] [:not B]] -> [:or [:and A]]"
  [query is-duplicate-clause]
  (let [clause-branch-type (first query)
        query-content (rest query)
        new-clause (if is-duplicate-clause
                     []
                     [clause-branch-type])]
    (into new-clause (combine-lists (mapv #(if (nested-clause? %)
                                             (cond (= (nth % 0) clause-branch-type)
                                                   (reduce-query % true)

                                                   (and (= clause-branch-type :or)
                                                        (= (nth % 0) :not))
                                                   []

                                                   :else [(reduce-query % false)])

                                             [%]) query-content)))))

(defn parse-query
  "Turn a query string into a list format that read-string can understand.
   
   e.g. {and: [[A]] [[B]]} -> ['and: '[[A]]' '[[B]]']
   "
  [query-str]
  (-> query-str
      (replace-braces-with-brackets)
      (branch-clause->keyword)
      (page-refs->strings 0)
      #_:clj-kondo/ignore
      (read-string)
      (reduce-query false)))

(defn- roam-native-and [refs]
  [[(list 'identity refs) '?refs]
   [(list 'identity refs) '[?ref ...]]
   '[?block :block/refs ?ref]
   '(roam-native ?block ?refs)])

(defn- roam-native-not [refs]
  [[(list 'identity refs) '?refs]
   '(roam-native ?block ?refs)])

(defn- resolve-roam-native-and [refs nested-clauses]
  (into (if (seq refs)
          (roam-native-and refs)
          [])
        (combine-lists nested-clauses)))

(defn- resolve-roam-native-not [refs nested-clauses]
  [(concat (concat (list 'not-join '[?block])
                   (if (seq refs)
                     (roam-native-not refs)
                     (list)))
           (combine-lists nested-clauses))])

(defn- resolve-roam-native-or [refs nested-clauses]
  [(concat
    (concat (list 'or-join '[?block])
            (mapv #(concat (list 'and) (roam-native-and [%])) refs))
    (map #(concat (list 'and) %) nested-clauses))])

(defn- resolve-roam-native-between [query-content]
  [(concat (list 'or-join '[?block])
           (resolve-between-clause query-content))])

(defn- resolve-roam-native-query [query]
  (let [clause-branch-type (nth query 0)
        query-content (subvec query 1)]
    (if (= clause-branch-type :between)
      (resolve-roam-native-between query-content)
      (let [nested-clauses (mapv #(resolve-roam-native-query %)
                                 (filter nested-clause? query-content))
            refs (->> (filter #(or (string? %)
                                   (list? %)) query-content)
                      (map ref->eid)
                      (flatten)
                      (vec))]
        (cond (= clause-branch-type :and)
              (resolve-roam-native-and refs nested-clauses)

              (= clause-branch-type :not)
              (resolve-roam-native-not refs nested-clauses)

              (= clause-branch-type :or)
              (resolve-roam-native-or refs nested-clauses))))))

(defn- roam-native-query->datalog [block-string]
  (-> block-string
      (trim-roam-native-query)
      (parse-query)
      (resolve-roam-native-query)
      (filter-query-blocks)))

(defn- execute-roam-native-query [current-blocks clauses]
  (let [query (->> (into '[:find [?block ...]
                           :in $ %
                           :where]
                         clauses)
                   (add-current-blocks-to-query current-blocks))]
    (rd/q query
          query-rules
          current-blocks)))

(def m-roam-native-query->datalog
  (memoize roam-native-query->datalog))

(defn roam-native-query [current-blocks block-string]
  (->> (m-roam-native-query->datalog block-string)
       (execute-roam-native-query current-blocks)))
