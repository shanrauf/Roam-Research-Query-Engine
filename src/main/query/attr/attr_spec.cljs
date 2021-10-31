(ns query.attr.attr-spec
  (:require [cljs.test :refer (deftest is testing)]
            [roam.test-graph :as graph]
            [query.attr.core :refer [execute-roam-attr-query execute-reverse-roam-attr-query]]
            [query.attr.operation :refer [get-operator]]
            [query.attr.value :refer [text-type num-type ref-type]]))

(deftest roam-attr-spec []
  (testing "Reverse Roam attributes"
    [(is (= (execute-reverse-roam-attr-query graph/type-attr graph/task-3)
            [graph/task-type]))
     (is (= (execute-reverse-roam-attr-query graph/status-attr graph/task-2)
            [graph/completed graph/october-28-2021]))])
  (testing "Returns properly whether single/multi-value attributes"
    [(is (= (execute-roam-attr-query graph/type-attr
                                     [[(get-operator :includes)
                                       [[graph/task-type ref-type]]]])
            [graph/task-1 graph/task-2 graph/task-3]))
     (is (= (execute-roam-attr-query graph/status-attr
                                     [[(get-operator :includes)
                                       [[graph/completed
                                         ref-type]]]])
            [graph/task-2 graph/task-3]))])
  (testing "Operators filter results properly:"
    (testing "String types"
      [(is (= (execute-roam-attr-query graph/description-attr
                                       [[(get-operator :includes)
                                         [["task 1" text-type]]]])
              [graph/task-1]))
       (is (= (execute-roam-attr-query graph/description-attr
                                       [[(get-operator :includes)
                                         [["task 2" text-type]]]])
              [graph/task-2]))
       (is (= (execute-roam-attr-query graph/description-attr
                                       [[(get-operator :=)
                                         [["This is task 1" text-type]]]])
              [graph/task-1]))])
    (testing "Number types"
      [(is (= (execute-roam-attr-query graph/priority-attr
                                       [[(get-operator :includes)
                                         [[1 num-type]]]])
              [graph/task-1]))
       (is (= (execute-roam-attr-query graph/priority-attr
                                       [[(get-operator :=)
                                         [[2 num-type]]]])
              [graph/task-2]))
       (is (= (execute-roam-attr-query graph/priority-attr
                                       [[(get-operator :>)
                                         [[2 num-type]]]])
              [graph/task-3]))
       (is (= (execute-roam-attr-query graph/priority-attr
                                       [[(get-operator :<)
                                         [[2 num-type]]]])
              [graph/task-1]))
       (is (= (execute-roam-attr-query graph/priority-attr
                                       [[(get-operator :includes)
                                         [[2 num-type]]]])
              [graph/task-2 graph/task-3]))
       (is (= (execute-roam-attr-query graph/priority-attr
                                       [[(get-operator :includes)
                                         [[3 num-type]]]])
              [graph/task-3]))])
    (testing "Ref types"
      [(is (= (execute-roam-attr-query graph/status-attr
                                       [[(get-operator :=)
                                         [[graph/in-progress ref-type]]]])
              [graph/task-1]))
       (is (= (execute-roam-attr-query graph/status-attr
                                       [[(get-operator :=)
                                         [[graph/completed ref-type]]]])
              []))])
    (testing "Date ref types"
      [(is (= (execute-roam-attr-query graph/deadline-attr
                                       [[(get-operator :is_dnp)
                                         []]])
              [graph/task-1 graph/task-2]))
       (is (= (execute-roam-attr-query graph/deadline-attr
                                       [[(get-operator :<)
                                         [[graph/nov-1-2021 ref-type]]]])
              [graph/task-1 graph/task-2]))
       (is (= (execute-roam-attr-query graph/deadline-attr
                                       [[(get-operator :>)
                                         [[graph/nov-1-2021 ref-type]]]])
              []))
       (is (= (execute-roam-attr-query graph/deadline-attr
                                       [[(get-operator :>)
                                         [[graph/october-26-2021 ref-type]]]])
              [graph/task-1]))
       (is (= (execute-roam-attr-query graph/deadline-attr
                                       [[(get-operator :<=)
                                         [[graph/october-26-2021 ref-type]]]])
              [graph/task-2]))
       (is (= (execute-roam-attr-query graph/deadline-attr
                                       [[(get-operator :>=)
                                         [[graph/october-26-2021 ref-type]]]])
              [graph/task-1 graph/task-2]))])))
