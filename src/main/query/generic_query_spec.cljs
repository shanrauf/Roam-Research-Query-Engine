(ns query.generic-query-spec
  (:require [cljs.test :refer (deftest is testing)]))

; NOW: Test queries with different ?current-blocks

(deftest roam-attr-spec []
  (testing "Roam attr"
    (println "Running test function:")
    (is (= 1 1))))
