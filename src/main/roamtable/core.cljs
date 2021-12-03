(ns roamtable.core
  (:require
   [reagent.dom :as d]
   [reagent.core :as r]
   [roam.datascript :as rd]
   [query.core :refer [generic-roam-query]]
   [query.attr.core :refer [parse-one-line-roam-attr
                            extract-attr-ref]]
   [query.attr.value :refer [attr-value->value
                             attr-value->type
                             ref-type]]
   [query.attr.api :refer [get-roam-attr-values]]
   [clojure.string :as str]))


(def test-data
  (r/atom
   [1 2 3 4 5 6]))

(def table-state
  (r/atom
   {:entries []
    :current-page 1
    :sort-by [:col-1 :ASC]}))

(defn- get-attrs [columns])

(defn roam-table []
  [:div.table-container
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

; TODO this doesn't work well... Roam needs better API functions
; for rendering (cuz I don't want to reverse engineer them...)
;; (defn rendered-node-component [uid]
;;   (r/create-class
;;    {:component-did-mount #(-> js/window
;;                               (.-roamAlphaAPI)
;;                               (.-ui)
;;                               (.-components)
;;                               (.renderBlock (clj->js {:uid uid
;;                                                       :el (-> js/document
;;                                                               (.getElementById uid))})))
;;     :reagent-render
;;     (fn []
;;       [:div {:id uid}])}))

(defn- eid->uid [eid]
  (ffirst (rd/q '[:find ?uid
                  :in $ ?eid
                  :where
                  [?eid :block/uid ?uid]]
                eid)))

(defn- get-block-label [eid]
  (ffirst (rd/q '[:find ?str
                  :in $ ?eid
                  :where
                  (or [?eid :node/title ?str]
                      [?eid :block/string ?str])]
                eid)))

(defn- format-attr-vals [attr-vals]
  (->> (map #(let [val (attr-value->value %)
                   attr-type (attr-value->type %)]
               (if (= attr-type ref-type)
                 (get-block-label val)
                 val)) attr-vals)
       (str/join ", ")))
; ^#\/app\/([^/]*?)(?:\/page\/.{9,10})?$
(defn get-graph-name []
  (let [url-hash (-> js/window
                     (.-location)
                     (.-hash))]
    (last (re-find #"^#\/app\/([^/]*?)(?:\/page\/.{9,10})?$" url-hash))))

(defn block-row-component [sorted-attr-keys block-eid attr-vals]
  (println attr-vals)
  [:tr
   ; TODO this shouldn't be hardcoded, you need a :block/label attribute you can parse
  ;;  [:td (get-block-label block-eid)]
   [:td [:a {:href (str "https://roamresearch.com/#/app/"
                        (get-graph-name)
                        "/page/"
                        (eid->uid block-eid))}
         (get-block-label block-eid)]]
   (map #(identity [:td (-> (get attr-vals %)
                            (format-attr-vals))])
        sorted-attr-keys)])

; For roam/render
#_:clj-kondo/ignore
(defn main [{:keys [block-uid]} & args]
  (println "{{roam/render: ((" block-uid "))}}")
  (println "Args: " args)
  ;; (println (. js/Date now))
  ;; (println "TEST")
  ;; (println (. js/Date now))
  (let [siblings (map first
                      (rd/q '[:find (pull ?c [:block/uid :node/title :db/id :block/string {:block/children ...} {:block/refs 2}])
                              :in $ ?uid
                              :where
                              [?e :block/uid ?uid]
                              [?e :block/parents ?p]
                              [?p :block/children ?e]
                              [?p :block/children ?c]]
                            block-uid))
        columns-block (-> (filterv #(str/includes? (:block/string %) "Columns::") siblings)
                          (first))
        query-uid (-> (filterv #(str/includes? (:block/string %) "query::") siblings)
                      (first)
                      (:block/uid))
        query-result (generic-roam-query query-uid)
        attributes (reduce #(let [[attr-title _] (parse-one-line-roam-attr (:block/string %2))
                                  attr-key (keyword (str (extract-attr-ref attr-title (:block/refs %2))))]
                              (assoc %1 attr-key attr-title)) {} (:block/children columns-block))
        sorted-attr-keys (sort (keys attributes))
        sorted-attr-titles (mapv #(get attributes %) sorted-attr-keys)
        table-result (get-roam-attr-values query-result (->> sorted-attr-keys
                                                             (map name)
                                                             (map int)))]
    [:div.table-container
     [:table.roam-table
      [:thead
       [:tr
        [:td {:style {:font-weight "bold"}} "Label"]
        (map #(identity [:th %])
             sorted-attr-titles)]]
      [:tbody
       (map #(block-row-component sorted-attr-keys
                                  (-> (name %)
                                      (int))
                                  (get table-result %))
            (keys table-result))]]]))

#_:clj-kondo/ignore
(defn init []
  (d/render [roam-table] (.getElementById js/document "root")))
