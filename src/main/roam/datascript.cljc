(ns roam.datascript
  (:require [datascript.core :as d]
            [roam.test-graph]))

(def schema (roam.test-graph/roam-graph :schema))
(def datoms (reduce #(conj %1 (apply d/datom %2)) #{} (roam.test-graph/roam-graph :datoms)))
(def db (d/conn-from-datoms datoms schema))

; NOTE: queries won't work without an :in clause for some reason
(defn- add-db-to-args [args]
  (if (> (count args) 1)
    (into [(first args) @db] (rest args))
    (conj args @db)))

(defn q [& args]
  (apply d/q (add-db-to-args args)))

(defn entity [eid]
  (d/entity @db eid))

;; (defn pull [& args]
;;   (apply d/pull (add-db-to-args args)))
