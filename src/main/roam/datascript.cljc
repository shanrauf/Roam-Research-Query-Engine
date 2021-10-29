(ns roam.datascript
  (:require [datascript.core :as d]
            [roam.test-graph]))

(defonce schema (roam.test-graph/roam-graph :schema))
(defonce datoms (reduce #(conj %1 (apply d/datom %2)) #{} (roam.test-graph/roam-graph :datoms)))
(defonce db (d/conn-from-datoms datoms schema))

(defn- add-db-to-args [args]
  (if (> (count args) 1)
    (into [(first args) @db] (rest args))
    (conj args @db)))

(defn q [& args]
  (apply d/q (add-db-to-args args)))
