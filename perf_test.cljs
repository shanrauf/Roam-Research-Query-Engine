(ns query.perf-test
  #_:clj-kondo/ignore
  (:require [taoensso.tufte :as tufte :refer (p defnp profiled)]
            [util.debug :as debug :refer [perf log defntraced]]))

; I paste this into query.core and run (performance-test) in ^:dev/after-load start.
; You can also manually run in the REPL
(defn test-func-1 []
  (reduce #(+ %1 %2) (range 50000)))

(defn test-func-2 []
  (dotimes [_ 1000000] (:a {:a 1})))

(defn performance-test []
  (js/console.log "Profiling...")
  (let [results
        (taoensso.tufte/format-pstats (second (profiled
                                               {}
                                               (dotimes [_# 5]
                                                 (p :test-1 (test-func-1))
                                                 (p :test-2 (test-func-2)))))
                                      {:columns [:n-calls :min :p50 :p90 :p99 :max :mean :mad :clock :total]
                                       :sort-fn (fn [m] (get m :sum))})]
    (js/console.log "%cResults:", "color: green; font-size: 20px; font-weight: bold;")
    (js/console.log results)))
