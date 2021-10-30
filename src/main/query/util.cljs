(ns query.util
  (:require goog.crypt goog.crypt.Md5
            [clojure.string :as str]))

(defonce branch-clauses ["and" "or" "not" "between"])
(defn branch? [branch]
  (some #(= branch %) branch-clauses))


(defn string->md5-hex
  "String goes in, md5 hex string comes out."
  [s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (goog.crypt/byteArrayToHex
   (let [md5 (goog.crypt.Md5.)]
     (.update md5 (goog.crypt/stringToUtf8ByteArray s))
     (.digest md5))))

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

(defn parse-roam-dnp-ref [title]
  (-> (subs title 2 (- (count title) 2))
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