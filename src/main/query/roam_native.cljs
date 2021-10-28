(ns query.roam-native
  (:require [clojure.string :as str]
            [cljs.reader :refer [read-string]]
            [query.util :refer [rand-str branch->datalog branch-clauses]]
            [query.errors :refer [throw-error roam-native-error]]))

(defonce roam-ref-regex #"\(\(([^()]+)\)\)|\[\[([^\[\]]+)\]\]")

(defn roam-native-query? [block-string]
  ; Replace `` if surrounding the block
  (let [query-str (if (and (str/starts-with? block-string "`")
                           (str/ends-with? block-string "`"))
                    (subs block-string 1 (- (count block-string) 1))
                    block-string)]
    (and (str/starts-with? query-str "{{")
         (str/ends-with? query-str "}}")
         ;; Don't confuse {{query}} with {{roam/render}}
         (-> (subs query-str 2 (- (count query-str) 2))
             (str/trim)
             (or (str/starts-with? query-str "[[query")
                 (str/starts-with? query-str "query"))))))

; When you find "[[", then keep going until you hit 0 count, then return how long the ref is.
;; throw error after like 500 recursions
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

(defn combine-lists [lists] ; todo idek if this works
  (reduce #(into %1 %2) [] lists))

(defn- resolve-between-clause [[date1 date2]]
  [date1 date2])

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

(defn refs->strings [query-str start]
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

; TODO Breaks if a page title has a curly brace or unbalanced brackets
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

; TODO actually make the datalog work b4 resolving to the datalog
(defn resolve-roam-native-query [query branch-type]
  (let [nested-branch (name (nth query 0))
        query-content (subvec query 1)
        ; TODO shouldn't this be >=
        has-many-and-refs (<= 2 (reduce #(if (and (or (str/starts-with? (str %2) "[[")
                                                      (str/starts-with? (str %2) "(("))
                                                  (re-find roam-ref-regex (str %2)))
                                           (+ %1 1)
                                           %1) 0 query-content))
        child-clauses (combine-lists (mapv #(if (keyword? (nth % 0))
                                              (resolve-roam-native-query % nested-branch)
                                              (let [random-free-var (symbol (str "?" (rand-str 4)))]
                                                ; random var names means no caching in future probably...
                                                [(list 'ref-to-eid (str %) random-free-var)
                                                 (list 'references? '?blocks random-free-var has-many-and-refs)])) query-content))]
    ; First, add a check for :between cuz that turns into a rule for every date in between.
    (if (= nested-branch "between")
      (resolve-between-clause query-content)
      (branch->datalog branch-type nested-branch child-clauses))))
