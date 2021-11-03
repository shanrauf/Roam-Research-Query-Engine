(ns roamtable.query
  #_:clj-kondo/ignore
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [roam.datascript :as rd]))

; YOUR CODE HERE...

(defn main [{:keys [block-uid]} & args]
  (println "{{roam/render: ((" block-uid "))}}")
  (println "Args: " args)
  (println (. js/Date now))
  (println "TEST")
  (println (. js/Date now))
  [:div "HERE"])