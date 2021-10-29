(ns query.roam-attribute-spec
  (:require [cljs.test :refer (deftest is testing)]
            [query.roam-attribute :refer [test-func]]))

(deftest roam-attr-spec []
  (testing "Roam attr"
    (println "Running test function:")
    (test-func)
    (is (= 1 1))))
