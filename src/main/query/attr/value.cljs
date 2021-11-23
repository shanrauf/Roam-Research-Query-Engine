(ns query.attr.value
  (:require [clojure.string :as str]
            [roam.datascript :as rd]
            [query.util :refer [dnp-title->date-str]]))

(defonce text-type :text-type)
(defonce num-type :num-type)
(defonce ref-type :ref-type)

; #[[Status]] | #Status | [[Status]] | (blockRef)
(defonce roam-ref-regex #"#\[\[.+\]\]|#[^\s]*|\(\(([^()]+)\)\)|\[\[([^\[\]]+)\]\]")
(defonce float-regex #"^\d+(\.\d+)?$")
(defonce dnp-title-regex #"(January|February|March|April|May|June|July|August|September|October|November|December) [0-3]?[0-9](st|nd|rd|th), [0-9][0-9][0-9][0-9]")

(defn- eid->title [eid]
  (ffirst (rd/q '[:find ?title
                  :in $ ?e
                  :where
                  [?e :node/title ?title]]
                eid)))

(defn attr-value->type [pair]
  (last pair))
(defn attr-value->value [pair]
  (first pair))

(defn attr-value->timestamp [attr-val]
  (let [date-title (-> (attr-value->value attr-val)
                       (eid->title))]
    (if date-title
      (->> date-title
           (dnp-title->date-str)
           (. js/Date parse))
      nil)))

(defn parse-attribute-ref-value
  "The attribute is a ref type if the value ONLY contains references"
  [original-value value refs]
  (if (re-find roam-ref-regex value)
    (parse-attribute-ref-value original-value
                               (-> (str/replace value roam-ref-regex "")
                                   (str/trim))
                               refs)
    (if (str/blank? value)
      (mapv #(identity [% ref-type]) refs)
      [[original-value text-type]])))

; TODO A nested page tag is not parsed properly: [[Page [[A]]]] becomes 3 values, not 1
; (low priority since practically, this barely affects query validity)
;; TODO Should you allow an attribute's value to be the attribute itself? e.g. Type:: [[Type]]
(defn extract-attr-values [input attribute input-refs]
  (let [value (str/trim input)
        ; Attribute is either a Roam attribute or a Datomic attribute
        refs (filterv #(not= attribute %) input-refs)]
    (cond
      (re-find float-regex value) [[(js/parseFloat value) num-type]]
      (and (seq refs)
           (re-find roam-ref-regex value)) (parse-attribute-ref-value value value refs)
      :else [[value text-type]])))
