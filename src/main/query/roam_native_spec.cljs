(ns query.roam-native-spec
  (:require [cljs.test :refer (deftest is testing)]
            [query.roam-native :refer [parse-query
                                       m-roam-native-query]]))

(defonce test-clauses-1 "{and: [[A]] [[B]]}")
(defonce test-query-1 (str "{{[[query]]: " test-clauses-1 "}}"))
(defonce test-1-parsed [:and "[[A]]" "[[B]]"])
(defonce test-1-result '[(ref-to-eid "[[A]]" ?A) (roam-native ?block ?A) (ref-to-eid "[[B]]" ?B) (roam-native ?block ?B)])

(defonce test-clauses-2 "{and: [[A]] ((oYNHxtIJ9))}")
(defonce test-query-2 (str "{{[[query]]: " test-clauses-2 "}}"))
(defonce test-2-parsed [:and "[[A]]" '((oYNHxtIJ9))])
(defonce test-2-result '[(ref-to-eid "[[A]]" ?A) (roam-native ?block ?A) (ref-to-eid "((oYNHxtIJ9))" ?oYNHxtIJ9) (roam-native ?block ?oYNHxtIJ9)])

(defonce test-clauses-3 "{and: {between: [[October 1st, 2021]] [[October 2nd, 2021]]} [[A]] {not: [[B]]}}")
(defonce test-query-3 (str "{{[[query]]: " test-clauses-3 "}}"))
(defonce test-3-parsed [:and [:between "[[October 1st, 2021]]" "[[October 2nd, 2021]]"] "[[A]]" [:not "[[B]]"]])
(defonce test-3-result '[(or (and [?e :block/uid "10-1-2021"] (roam-native ?block ?e)) (and [?e :block/uid "10-2-2021"] (roam-native ?block ?e))) (ref-to-eid "[[A]]" ?A) (roam-native ?block ?A) not (ref-to-eid "[[B]]" ?B) (roam-native ?block ?B)])

(deftest roam-native-parsing-spec []
  (testing "Roam native queries are properly parsed"
    [(is (= (parse-query test-clauses-1) test-1-parsed))
     (is (= (parse-query test-clauses-2) test-2-parsed))
     (is (= (parse-query test-clauses-3) test-3-parsed))]))


(deftest roam-native-spec []
  (is (= (m-roam-native-query test-query-1) test-1-result))
  (is (= (m-roam-native-query test-query-2) test-2-result))
  (is (= (m-roam-native-query test-query-3) test-3-result)))
