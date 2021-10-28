(ns roam.datascript-spec
  (:require [cljs.test :refer (deftest is testing)]
            [roam.datascript :refer [init]]))


(deftest datascript-mock-spec []
  (testing "Roam native queries are properly parsed"
    [(is (= (init) true))]))

; TODO test nested refs in pages
;; todo filter query automatically
;; (deftest roam-native-spec [])
