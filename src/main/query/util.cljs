(ns query.util
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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

; TODO Why didn't Javascript's .indexOf work? Go check again.
(defn- index-of
  "ClojureScript replacement for .indexOf, which won't work in {{roam/render}}"
  [s v]
  (loop [idx 0 items s]
    (cond
      (empty? items) nil
      (= v (first items)) idx
      :else (recur (inc idx) (rest items)))))

(defn add-current-blocks-to-query [query current-blocks]
  (let [where-idx (index-of query :where)
        new-query (vec-insert query where-idx '?current-blocks)]
    (if (seq current-blocks)
      (vec-insert new-query (+ where-idx 2) '[(ground ?current-blocks) [?block ...]])
      new-query)))

(defn filter-query-blocks [where-clauses]
  (into where-clauses '[(not-join [?block]
                                  [?query-ref :node/title "query"]
                                  (or-join
                                   [?block ?query-ref]
                                   [?block :block/refs ?query-ref]
                                   (and [?block :block/parents ?parents]
                                        [?parents :block/refs ?query-ref])))]))

(defn filter-table-column-specs [where-clauses]
  (into where-clauses '[(not-join [?block]
                                  [?spec-ref :node/title "Table Columns"]
                                  (or-join
                                   [?block ?spec-ref]
                                   [?block :block/refs ?spec-ref]
                                   (and [?block :block/parents ?parents]
                                        [?parents :block/refs ?spec-ref])))]))

(defn remove-backticks [block-string]
  (if (and (str/starts-with? block-string "`")
           (str/ends-with? block-string "`"))
    (subs block-string 1 (- (count block-string) 1))
    block-string))

; NOTE: breaks with unbalanced brackets e.g. [[Page [ ABC]]
; Idc to fix because Roam will expose parser eventually
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

(defonce text-datomic-attrs
  #{:block/string
    :create/email
    :node/title
    :block/text-align
    :block/uid
    :children/view-type
    :edit/email})

(defonce num-datomic-attrs
  #{:create/time
    :edit/time
    :block/order
    :block/heading
    :db/id})

(defonce reverse->block-datomic-attrs
  {:attrs/_lookup :attrs/lookup
   :block/_children :block/children
   :block/_page :block/page
   :block/_parents :block/parents
   :block/_refs :block/refs})

(defonce reverse-block-datomic-attrs
  (set (into (keys reverse->block-datomic-attrs)
             (vals reverse->block-datomic-attrs))))

(defonce block-datomic-attrs
  (set/union reverse-block-datomic-attrs
             #{:attrs/lookup
               :block/children
               :block/page
               :block/parents
               :block/refs
               :edit/seen-by
               :edit/user}))

; NOTE: Missing :block/open because I don't have a boolean type right now.
(defonce datomic-attrs
  (set/union text-datomic-attrs
             num-datomic-attrs
             block-datomic-attrs))
