(ns roamtable.core
  (:require
   [reagent.dom :as d]
   [roamtable.views.counter :as counter]))

(defn roam-table []
  [:div
   [:h1 "Roam Table"]
   [counter/counter-component]])

(defn init []
  (d/render [roam-table] (.getElementById js/document "root")))
