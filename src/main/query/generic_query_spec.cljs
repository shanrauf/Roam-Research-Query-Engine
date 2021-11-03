(ns query.generic-query-spec
  (:require [cljs.test :refer (deftest is testing)]
            [query.core :refer [generic-roam-query]]))

; NOW: Test queries with different ?current-blocks

(deftest roam-attr-spec []
  (testing "Roam attr"
    ;; (println "Running test function:")
    ;; (println (. js/Date now))
    ;; (println (generic-roam-query "cHdjj4_5e"))
    ;; (println (. js/Date now))
    (is (= 1 1))))
