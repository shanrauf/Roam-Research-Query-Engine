(ns query.roam-native-spec
  (:require [cljs.test :refer (deftest is testing)]
            [query.roam-native :refer [parse-query
                                       m-roam-native-query]]))

(defonce test-clauses-1 "{and: [[A]] [[B]]}")
(defonce test-query-1 (str "{{[[query]]: " test-clauses-1 "}}"))
(defonce test-1-parsed [:and "[[A]]" "[[B]]"])
(defonce test-1-result '[(ref-to-eid "[[A]]" ?7fc56270e7a70fa81a5935b72eacbe29) (roam-native ?block ?7fc56270e7a70fa81a5935b72eacbe29) (ref-to-eid "[[B]]" ?9d5ed678fe57bcca610140957afab571) (roam-native ?block ?9d5ed678fe57bcca610140957afab571) [?query-ref :node/title "query"] (not [?block :block/refs ?query-ref])])

(defonce test-clauses-2 "{and: [[A]] ((oYNHxtIJ9))}")
(defonce test-query-2 (str "{{[[query]]: " test-clauses-2 "}}"))
(defonce test-2-parsed [:and "[[A]]" '((oYNHxtIJ9))])
(defonce test-2-result '[(ref-to-eid "[[A]]" ?7fc56270e7a70fa81a5935b72eacbe29) (roam-native ?block ?7fc56270e7a70fa81a5935b72eacbe29) (ref-to-eid "((oYNHxtIJ9))" ?e6b47e76814112b1e271a96468a911c8) (roam-native ?block ?e6b47e76814112b1e271a96468a911c8) [?query-ref :node/title "query"] (not [?block :block/refs ?query-ref])])

(defonce test-clauses-3 "{and: {between: [[October 1st, 2021]] [[October 2nd, 2021]]} [[A]] {not: [[B]]}}")
(defonce test-query-3 (str "{{[[query]]: " test-clauses-3 "}}"))
(defonce test-3-parsed [:and [:between "[[October 1st, 2021]]" "[[October 2nd, 2021]]"] "[[A]]" [:not "[[B]]"]])
(defonce test-3-result '[(or (and [?e :block/uid "10-1-2021"] (roam-native ?block ?e)) (and [?e :block/uid "10-2-2021"] (roam-native ?block ?e))) (ref-to-eid "[[A]]" ?7fc56270e7a70fa81a5935b72eacbe29) (roam-native ?block ?7fc56270e7a70fa81a5935b72eacbe29) not (ref-to-eid "[[B]]" ?9d5ed678fe57bcca610140957afab571) (roam-native ?block ?9d5ed678fe57bcca610140957afab571) [?query-ref :node/title "query"] (not [?block :block/refs ?query-ref])])

(deftest roam-native-parsing-spec []
  (testing "Roam native queries are properly parsed"
    [(is (= (parse-query test-clauses-1) test-1-parsed))
     (is (= (parse-query test-clauses-2) test-2-parsed))
     (is (= (parse-query test-clauses-3) test-3-parsed))]))


(deftest roam-native-spec []
  (is (= (m-roam-native-query test-query-1) test-1-result))
  (is (= (m-roam-native-query test-query-2) test-2-result))
  (is (= (m-roam-native-query test-query-3) test-3-result)))
