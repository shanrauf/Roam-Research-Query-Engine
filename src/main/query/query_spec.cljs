(ns query.query-spec
  (:require [cljs.test :refer (deftest is testing)]
            [query.roam-native :refer [parse-query refs->strings]]))

(deftest this-spec []
  (is (= 1 1)))

(defonce test-roam-native-query "{and: [[A]] [[B]]}")

(deftest roam-native-parsing-spec []
  (testing "Page references are turned into strings"
    [(is (= (refs->strings "[:and [[A]] [[B]]]" 0) "[:and \"[[A]]\" \"[[B]]\"]"))
     (is (= (refs->strings "[:and [[A]] [:not [[B]]]]" 0) "[:and \"[[A]]\" [:not \"[[B]]\"]]"))
     (is (= (refs->strings "[:and [[[[Nested]] A]] [:not [[B]]]]" 0) "[:and \"[[[[Nested]] A]]\" [:not \"[[B]]\"]]"))])
  (testing "Roam native queries are properly parsed"
    [(is (= (parse-query test-roam-native-query) [:and "[[A]]" "[[B]]"]))]))
