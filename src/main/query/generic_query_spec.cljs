(ns query.generic-query-spec
  (:require [cljs.test :refer (deftest is testing)]
            [roam.test-graph :as graph]
            [query.core :refer [generic-roam-query]]))

(deftest roam-attr-spec []
  (testing "Roam attr"
    [(is (= (set (generic-roam-query graph/generic-query-1-uid))
            #{graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-2-uid))
            #{graph/task-1}))
     (is (= (set (generic-roam-query graph/generic-query-3-uid))
            #{graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-4-uid))
            #{graph/task-2 graph/task-3}))
     (is (= (set (generic-roam-query graph/generic-query-5-uid))
            #{graph/task-1 graph/task-2 graph/task-3}))
     (is (= (set (generic-roam-query graph/generic-query-6-uid))
            #{}))
     (is (= (set (generic-roam-query graph/generic-query-7-uid))
            #{graph/task-1 graph/task-2 graph/task-3}))
     (is (= (set (generic-roam-query graph/generic-query-8-uid))
            #{graph/task-1 graph/task-3}))
     (is (= (set (generic-roam-query graph/generic-query-9-uid))
            #{graph/task-1}))
     (is (= (set (generic-roam-query graph/generic-query-10-uid))
            #{graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-11-uid))
            #{graph/task-1 graph/task-2}))
     (is (= (set (generic-roam-query graph/generic-query-12-uid))
            #{graph/task-1}))]))
