(ns roam.datascript
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [clojure.edn]
            [datascript.core :as d]
            [roam.test-graph]))

(def db (atom nil))

; Development/testing DB
(reset! db (d/conn-from-datoms (reduce #(conj %1 (apply d/datom %2)) #{} (roam.test-graph/roam-graph :datoms))
                               (roam.test-graph/roam-graph :schema)))

; Production speed testing: put the edn file in public/
;; (go (if (not= @db nil)
;;       nil ; Reload app to re-init DB since initialization is very slow for big graphs
;;       (let [response (<! (http/get "http://localhost:8080/srauf.edn"
;;                                    {:with-credentials? false}))
;;             roam-graph (:body response)]
;;         (reset! db (d/conn-from-db roam-graph))
;;         (println "Initialized db."))))

; NOTE: queries won't work without an :in clause for some reason
; This is never an issue for me so for now it's a feature not a bug.
(defn- add-db-to-args [args]
  (if (> (count args) 1)
    (into [(first args) @@db] (rest args))
    (conj args @@db)))

(defn q [& args]
  (apply d/q (add-db-to-args args)))


(defn entity [eid]
  (d/entity @@db eid))

(defn pull [& args]
  (apply d/pull (add-db-to-args args)))
