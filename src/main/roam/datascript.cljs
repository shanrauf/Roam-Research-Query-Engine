(ns roam.datascript
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [clojure.edn]
            [datascript.core :as d]
            [roam.test-graph]))

(def db (atom nil))
(def use-custom-graph false)

(defn- get-graph-url
  "Put custom edn files in public/ & replace srauf"
  []
  (if use-custom-graph
    "http://localhost:8080/srauf.edn"
    "http://localhost:8080/test-graph.edn"))

(defn init-db+ []
  (js/Promise. (fn [resolve _]
                 (go (if (not= @db nil)
                       nil ; Don't re-init DB (slow for big, custom graphs); reload app if you need this
                       (let [url (get-graph-url)
                             response (<! (http/get url
                                                    {:with-credentials? false}))
                             roam-graph (:body response)]
                         (reset! db (d/conn-from-db roam-graph))
                         (println (str "Initialized db at " url))
                         (resolve true)))))))

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

; NOTE: As Josh said, pull is much slower than just using
; entity or querying or something so I won't be using this.
(defn pull [& args]
  (apply d/pull (add-db-to-args args)))
