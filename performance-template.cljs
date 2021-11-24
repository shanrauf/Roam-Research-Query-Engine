(ns query.perf
  (:require [datascript.core]
            [taoensso.tufte :as tufte :refer (p profiled)]
            [roam.datascript]))

; I just copy/pasted these files into `query.attr.core` and ran the perf test from the JS console (since Calva times out & is more clunky)

; From personal graph
(defonce type-attr 93733)
(defonce todos-attr 91924)
(defonce description-attr 11169)
(defonce status-attr 3562)
(defonce completed 732)
(defonce task-type 119742)
(defonce in-progress 18385)
(defonce project-type 27363)

(def all-attrs [type-attr todos-attr description-attr status-attr])
(def all-tasks (->> (datascript.core/q '[:find [?block ...]
                                         :in $
                                         :where
                                         [?block :attrs/lookup 119742]
                                         [?block :attrs/lookup 93733]] @@roam.datascript/db)
                    ; let's say pagination will be 50/page
                    (take 300)))
(def task-count (count all-tasks))

(defn test-func-1 []
  true)

(defn test-func-2 []
  true)

(defn performance-test []
  (println (taoensso.tufte/format-pstats (second (profiled
                                                  {}
                                                  (dotimes [_# 10]
                                                    (p :test-1 (test-func-1))
                                                    (p :test-2 (test-func-2))))) {:columns [:n-calls :min :p50 :p90 :p99 :max :mean :mad :clock :total]
                                                                                  :sort-fn (fn [m#] (get m# :sum))})))