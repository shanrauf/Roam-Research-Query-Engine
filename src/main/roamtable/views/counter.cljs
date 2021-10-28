(ns roamtable.views.counter
  (:require [reagent.core :as r]))

(defonce click-count (r/atom 0))

(defn counter-component []
  [:div
   [:h2 "Roam Table"]
   [:div.container
    [:input {:disabled true
             :value (str @click-count)}]
    [:button {:on-click #(swap! click-count inc)} "Count"]]])
