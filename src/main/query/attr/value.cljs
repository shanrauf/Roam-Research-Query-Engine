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
  (->> (attr-value->value attr-val)
       (eid->title)
       (dnp-title->date-str)
       (. js/Date parse)))

(defn parse-attribute-ref-value
  "The attribute is a ref type if the value ONLY contains references"
  [original-value value ref]
  (if (re-find roam-ref-regex value)
    (parse-attribute-ref-value original-value
                               (-> (str/replace value roam-ref-regex "")
                                   (str/trim))
                               ref)
    (if (= "" value)
      [ref ref-type]
      [original-value text-type])))

(defn parse-attribute-value [input attr-ref ref]
  (let [value (str/trim input)]
    (cond
      (re-find float-regex value) [(js/parseFloat value)
                                   num-type]
      (re-find roam-ref-regex value) (if (= attr-ref ref)
                                       nil
                                       (parse-attribute-ref-value value
                                                                  value
                                                                  ref))
      :else [value text-type])))

