(ns roam.datascript-spec
  (:require [cljs.test :refer (deftest is testing)]))


(deftest datascript-mock-spec []
  (testing "Roam native queries are properly parsed"
    [(is (= 1 1))]))

; TODO test nested refs in pages
;; todo filter query automatically
;; (deftest roam-native-spec [])
