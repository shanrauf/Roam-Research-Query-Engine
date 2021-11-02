(ns query.roam-native-spec
  (:require [cljs.test :refer (deftest is testing)]
            [query.roam-native :refer [parse-query
                                       roam-native-query]]))

(defonce test-clauses-1 "{and: [[A]] [[B]]}")
(defonce test-query-1 (str "`{{[[query]]: " test-clauses-1 "}}`"))
(defonce test-1-parsed [:and "[[A]]" "[[B]]"])
(defonce test-1-result #{42 47 123 135 138 139 171})

(defonce test-clauses-2 "{and: [[A]] ((SRrFMjN8P))}")
(defonce test-query-2 (str "{{[[query]]: " test-clauses-2 "}}"))
(defonce test-2-parsed [:and "[[A]]" '((SRrFMjN8P))])
(defonce test-2-result #{171})

(defonce test-clauses-3 "{and: {between: [[October 27th, 2021]] [[October 28th, 2021]]} [[A]] {not: [[B]]}}")
(defonce test-query-3 (str "{{[[query]]: " test-clauses-3 "}}"))
(defonce test-3-parsed [:and [:between "[[October 27th, 2021]]" "[[October 28th, 2021]]"] "[[A]]" [:not "[[B]]"]])
(defonce test-3-result #{})

(defonce test-clauses-4 "{and: [[A]] {and: [[B]]}}")
(defonce test-query-4 (str "{{[[query]]: " test-clauses-4 "}}"))
(defonce test-4-parsed [:and "[[A]]" "[[B]]"])
(defonce test-4-result #{42 47 123 135 138 139 171})

(defonce test-clauses-5 "{and: [[A]] {not: [[B]]}}")
(defonce test-query-5 (str "{{[[query]]: " test-clauses-5 "}}"))
(defonce test-5-parsed [:and "[[A]]" [:not "[[B]]"]])
(defonce test-5-result #{45 122 134})

(defonce test-clauses-6 "{or: [[A]] [[B]]}")
(defonce test-query-6 (str "{{[[query]]: " test-clauses-6 "}}"))
(defonce test-6-parsed [:or "[[A]]" "[[B]]"])
(defonce test-6-result #{52 42 135 55 139 170 171 122 123 138 45 47 134})

(defonce test-clauses-7 "{and: {between: [[October 27th, 2021]] [[October 28th, 2021]]}}")
(defonce test-query-7 (str "{{[[query]]: " test-clauses-7 "}}"))
(defonce test-7-parsed [:and [:between "[[October 27th, 2021]]" "[[October 28th, 2021]]"]])
(defonce test-7-result #{56 67})

(defonce test-clauses-8 "{or: {and: [[October 27th, 2021]] {not: [[B]]}}}")
(defonce test-query-8 (str "{{[[query]]: " test-clauses-8 "}}"))
(defonce test-8-parsed [:or [:and "[[October 27th, 2021]]" [:not "[[B]]"]]])
(defonce test-8-result #{56})

; Roam filters out NOT clauses in OR
(defonce test-clauses-9 "{or: {and: [[October 27th, 2021]]} {not: [[B]]}}")
(defonce test-query-9 (str "{{[[query]]: " test-clauses-9 "}}"))
(defonce test-9-parsed [:or [:and "[[October 27th, 2021]]"]])
(defonce test-9-result #{56})

(deftest roam-native-spec []
  (testing "Roam native query parsing"
    [(is (= (parse-query test-clauses-1) test-1-parsed))
     (is (= (parse-query test-clauses-2) test-2-parsed))
     (is (= (parse-query test-clauses-3) test-3-parsed))
     (is (= (parse-query test-clauses-4) test-4-parsed))
     (is (= (parse-query test-clauses-5) test-5-parsed))
     (is (= (parse-query test-clauses-6) test-6-parsed))
     (is (= (parse-query test-clauses-7) test-7-parsed))
     (is (= (parse-query test-clauses-8) test-8-parsed))
     (is (= (parse-query test-clauses-9) test-9-parsed))])
  (testing "Roam native results"
    [(is (= (set (roam-native-query [] test-query-1)) test-1-result))
     (is (= (set (roam-native-query [] test-query-2)) test-2-result))
     (is (= (set (roam-native-query [] test-query-3)) test-3-result))
     (is (= (set (roam-native-query [] test-query-4)) test-4-result))
     (is (= (set (roam-native-query [] test-query-5)) test-5-result))
     (is (= (set (roam-native-query [] test-query-6)) test-6-result))
     (is (= (set (roam-native-query [] test-query-7)) test-7-result))
     (is (= (set (roam-native-query [] test-query-8)) test-8-result))
     (is (= (set (roam-native-query [] test-query-9)) test-9-result))]))
