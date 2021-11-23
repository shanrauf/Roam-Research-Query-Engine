(ns query.generic-query-spec
  (:require [cljs.test :refer (deftest is testing)]
            [roam.test-graph :as graph]
            [query.core :refer [generic-roam-query]]))

(deftest roam-attr-spec []
  (testing "Generic Roam queries"
    [(is (= (set (generic-roam-query graph/generic-query-1-uid))
            #{graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-2-uid))
            #{graph/task-1}))
     (is (= (set (generic-roam-query graph/generic-query-3-uid))
            #{graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-4-uid))
            #{graph/task-2 graph/task-3}))
     (is (= (set (generic-roam-query graph/generic-query-5-uid))
            #{graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-6-uid))
            #{}))
     (is (= (set (generic-roam-query graph/generic-query-7-uid))
            #{graph/task-1 graph/task-3}))
     (is (= (set (generic-roam-query graph/generic-query-8-uid))
            #{graph/task-1}))
     (is (= (set (generic-roam-query graph/generic-query-9-uid))
            #{graph/task-1}))
     (is (= (set (generic-roam-query graph/generic-query-10-uid))
            #{graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-11-uid))
            #{graph/task-1 graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-12-uid))
            #{graph/task-1}))
     (is (= (set (generic-roam-query graph/generic-query-13-uid))
            #{graph/task-1}))
     (is (= (set (generic-roam-query graph/generic-query-14-uid))
            #{graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-15-uid))
            #{graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-16-uid))
            #{graph/task-1}))
     (is (= (set (generic-roam-query graph/generic-query-17-uid))
            #{graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-18-uid))
            #{graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-19-uid))
            #{graph/task-3}))
     (is (= (set (generic-roam-query graph/generic-query-20-uid))
            #{graph/october-31-2021 graph/nov-3-2021}))
     (is (= (set (generic-roam-query graph/generic-query-21-uid))
            #{graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-22-uid))
            #{graph/october-31-2021}))
     (is (= (set (generic-roam-query graph/generic-query-23-uid))
            #{graph/task-1 graph/task-2 graph/task-3}))
     (is (= (set (generic-roam-query graph/generic-query-24-uid))
            #{graph/task-1 graph/task-2 graph/task-3}))
     (is (= (set (generic-roam-query graph/generic-query-25-uid))
            #{graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-26-uid))
            #{graph/task-1}))
     (is (= (set (generic-roam-query graph/generic-query-27-uid))
            #{graph/task-1}))
     (is (= (set (generic-roam-query graph/generic-query-28-uid))
            #{graph/task-1}))
     (is (= (set (generic-roam-query graph/generic-query-29-uid))
            #{graph/task-1}))
     (is (= (set (generic-roam-query graph/generic-query-30-uid))
            #{graph/task-1}))]))
