(ns roamtable.core
  (:require
   [reagent.dom :as d]
   [reagent.core :as r]
   [roam.datascript :as rd]
   [query.attr.core :refer [reverse-roam-attr-query?
                            attr-values-rule
                            eval-roam-attr-query
                            single-value-attr?
                            identity-aggregate]]
   [query.util :refer [datomic-attrs]]
   [query.attr.value :refer [extract-attr-values]]
   [clojure.string :as str]))

(def test-headers
  (r/atom
   [{:name "Header1"
     :key :col-1}
    {:name "Header2"
     :key :col-2}
    {name "Header3"
     :key :col-3}]))

(defn- eid->ref-str [eid]
  true)

(defn- mixed-type? [cell-values]
  true)

(defn- get-columns-spec [spec-uid]
  (->> (rd/q '[:find (pull ?columns [:db/id
                                     :block/order
                                     :block/uid
                                     :node/title
                                     :block/string
                                     {:block/refs 2}
                                     {:block/children ...}])
               :in $ ?spec-uid
               :where
               [?spec :block/uid ?spec-uid]
               [?spec :block/children ?columns]]
             spec-uid)
       (map first)))

(defn- roam-attr? [block-string]
  (-> (str/trim block-string)
      (str/ends-with? "::")))

(defn- keyword-str->keyword [str]
  (keyword (subs str 1)))

(defn- datomic-attr? [attr]
  (contains? datomic-attrs attr))

(defn- datomic-attr->col-name [attr]
  (when (not (datomic-attr? attr))
    (throw (js/Error. (str attr " is not a supported attribute."))))
  (cond (= attr :edit/time)
        "Last Edited At"
        (= attr :create/time)
        "Created At"
        (= attr :create/email)
        "Created By"
        (= attr :edit/email)
        "Last Edited By"
        :else (-> (name attr)
                  (str/capitalize))))

(defonce roam-attr-spec :roam-attr)
(defonce reverse-roam-attr-spec :reverse-roam-attr)
(defonce datomic-attr-spec :datomic-attr)


(defn- columns->columns-spec [columns]
  (reduce #(let [block-string (-> (:block/string %2)
                                  (str/trim))
                 ref (:block/refs %2)
                 ref-title (:node/title ref)
                 ref-id (:db/id ref)
                 col-id (keyword (str "col-" (first %1)))]
             (cond (roam-attr? block-string)
                   [col-id ref-title roam-attr-spec ref-id]

                   (reverse-roam-attr-query? block-string)
                   [col-id ref-title reverse-roam-attr-spec ref-id]

                   (datomic-attr? (keyword-str->keyword block-string))
                   (let [datomic-attr (keyword-str->keyword block-string)]
                     [col-id
                      (datomic-attr->col-name datomic-attr)
                      datomic-attr-spec
                      datomic-attr])

                   :else (let [child (first (:block/children %2))
                               child-str (:block/string child)
                               child-ref (:block/refs child)
                               child-ref-title (:node/title child-ref)
                               child-ref-id (:db/id child-ref)]
                           (cond (roam-attr? child-str)
                                 [col-id child-ref-title roam-attr-spec child-ref-id]
                                 (reverse-roam-attr-query? child-str)
                                 [col-id child-ref-title reverse-roam-attr-spec child-ref-id]

                                 (datomic-attr? (keyword-str->keyword child-str))
                                 (let [datomic-attr (keyword-str->keyword child-str)]
                                   [col-id
                                    (datomic-attr->col-name datomic-attr)
                                    datomic-attr-spec
                                    datomic-attr])

                                 :else (throw (js/Error. "Unknown column spec")))))) [1 []] columns))

(defn- columns-spec->column-headers [spec]
  (map first spec))

(defn- extract-attrs-from-spec [spec]
  (reduce #(let [[col-id _ spec-type value] %2]
             (assoc %1 spec-type (conj (spec-type %1)
                                       [col-id value])))
          {roam-attr-spec []
           reverse-roam-attr-spec []
           datomic-attr-spec []}
          spec))

;; (defn- reverse-query [input-block attr]
;;   (eval-roam-attr-query [] attr [] [input-block])
;;   (rd/q '[:find ?block (aggregate ?identity ?v)
;;           :in $ % ?input-block ?identity ?is-single-value ?parse-attribute-value
;;           :where
;;           [?input-block :attrs/_lookup ?block]
;;           [?block :attrs/lookup ?attribute]
;;           (attr-values ?block ?attribute ?v ?is-single-value ?parse-attribute-value)]
;;         [attr-values-rule] input-block identity-aggregate single-value-attr? parse-attribute-value))

; extract-attrs-from-spec
; Then generate 1 query for attributes and do one (rd/pull) for datomic attrs
;; for attributes, i think do :find ?attribute (aggregate ?v) :in ?input-block-eid
;;; Not sure how to do reverse roam attrs. i guess [?block :attrs/_lookup ?e] (attr-values ?e) then parse/check
(defn- pull [eid columns-spec]
  (-> columns-spec
      (extract-attrs-from-spec))
  ;; (reduce #() {:eid eid} columns-spec)
  true)

;; {:eid 1
;;  :col-1 [["HERE" :text-type]]
;;  :col-2 [[123 :ref-type] [321 :ref-type]]
;;  :col-3 [[1 :num-type]]}

(def test-data
  (r/atom
   [1 2 3 4 5 6]))

(def table-state
  (r/atom
   {:entries []
    :current-page 1
    :sort-by [:col-1 :ASC]}))

(defn roam-table []
  [:div.tabe-container
   [:table.roam-table
    [:thead
     [:tr
      [:th "Header1"]
      [:th "Header2"]
      [:th "Header3"]]]
    [:tbody
     [:tr
      [:td "Cell1"]
      [:td "Cell2"]
      [:td "Cell 3"]]]]])

#_:clj-kondo/ignore
(defn init []
  (d/render [roam-table] (.getElementById js/document "root")))
