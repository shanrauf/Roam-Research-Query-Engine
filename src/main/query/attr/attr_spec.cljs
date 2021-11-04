(ns query.attr.attr-spec
  (:require [cljs.test :refer (deftest is testing)]
            [roam.test-graph :as graph]
            [query.attr.core :refer [eval-roam-attr-query
                                     eval-ref-datomic-query
                                     eval-reverse-roam-attr-query]]
            [query.attr.operation :refer [get-operator]]
            [query.attr.value :refer [text-type num-type ref-type parse-attribute-value]]))


(deftest roam-attr-spec []
  (testing "Attribute value parsing:"
    (testing "Ref type"
      [(is (= (parse-attribute-value "[[Status]]" graph/type-attr graph/status-attr)
              [graph/status-attr ref-type]))
       (is (= (parse-attribute-value "#Status" graph/type-attr graph/status-attr)
              [graph/status-attr ref-type]))
       (is (= (parse-attribute-value "#[[Status]]" graph/type-attr graph/status-attr)
              [graph/status-attr ref-type]))]))
  (testing "Reverse Roam attributes"
    [(is (= (set (eval-reverse-roam-attr-query [] graph/type-attr graph/task-3))
            #{graph/task-type}))
     (is (= (set (eval-reverse-roam-attr-query [] graph/todos-attr graph/task-1))
            #{graph/task-2 graph/task-3}))
     (is (= (set (eval-reverse-roam-attr-query [] graph/status-attr graph/task-2))
            #{graph/completed graph/october-28-2021}))])
  (testing "Ref Datomic attributes"
    [(is (= (set (eval-ref-datomic-query [] (:db/id graph/test-block-0) :block/children))
            (set (:block/children graph/test-block-0))))
     (is (= (set (eval-ref-datomic-query [] (:db/id graph/test-block-1) :block/children))
            (set (:block/children graph/test-block-1))))
     (is (= (set (eval-ref-datomic-query [] (:db/id graph/test-block-2) :block/parents))
            (set (:block/parents graph/test-block-2))))])
  (testing "Returns properly whether single/multi-value attributes"
    [(is (= (set (eval-roam-attr-query []
                                       graph/type-attr
                                       [[(get-operator :includes)
                                         [[graph/task-type ref-type]]]]
                                       [graph/task-type]))
            #{graph/task-1 graph/task-2 graph/task-3}))
     (is (= (set (eval-roam-attr-query []
                                       graph/status-attr
                                       [[(get-operator :includes)
                                         [[graph/completed
                                           ref-type]]]]
                                       [graph/completed]))
            #{graph/task-2 graph/task-3}))
     (is (= (set (eval-roam-attr-query []
                                       graph/todos-attr
                                       [[(get-operator :includes)
                                         [[graph/task-2
                                           ref-type]
                                          [graph/task-3
                                           ref-type]]]]
                                       [graph/task-2 graph/task-3]))
            #{graph/task-1}))
     (is (= (set (eval-roam-attr-query []
                                       graph/todos-attr
                                       [[(get-operator :=)
                                         [[graph/task-2
                                           ref-type]
                                          [graph/task-3
                                           ref-type]]]]
                                       [graph/task-2 graph/task-3]))
            #{graph/task-1}))])
  (testing "Operators filter results properly:"
    (testing "String types"
      [(is (= (set (eval-roam-attr-query []
                                         graph/description-attr
                                         [[(get-operator :includes)
                                           [["task 1" text-type]]]]
                                         nil))
              #{graph/task-1}))
       (is (= (set (eval-roam-attr-query []
                                         graph/description-attr
                                         [[(get-operator :includes)
                                           [["task 2" text-type]]]]
                                         nil))
              #{graph/task-2}))
       (is (= (set (eval-roam-attr-query []
                                         graph/description-attr
                                         [[(get-operator :=)
                                           [["This is task 1" text-type]]]]
                                         nil))
              #{graph/task-1}))])
    (testing "Number types"
      [(is (= (set (eval-roam-attr-query []
                                         graph/priority-attr
                                         [[(get-operator :includes)
                                           [[1 num-type]]]]
                                         nil))
              #{graph/task-1}))
       (is (= (set (eval-roam-attr-query []
                                         graph/priority-attr
                                         [[(get-operator :=)
                                           [[2 num-type]]]]
                                         nil))
              #{graph/task-2}))
       (is (= (set (eval-roam-attr-query []
                                         graph/priority-attr
                                         [[(get-operator :>)
                                           [[2 num-type]]]]
                                         nil))
              #{graph/task-3}))
       (is (= (set (eval-roam-attr-query []
                                         graph/priority-attr
                                         [[(get-operator :<)
                                           [[2 num-type]]]]
                                         nil))
              #{graph/task-1}))
       (is (= (set (eval-roam-attr-query []
                                         graph/priority-attr
                                         [[(get-operator :includes)
                                           [[2 num-type]]]]
                                         nil))
              #{graph/task-2 graph/task-3}))
       (is (= (set (eval-roam-attr-query []
                                         graph/priority-attr
                                         [[(get-operator :includes)
                                           [[3 num-type]]]]
                                         nil))
              #{graph/task-3}))])
    (testing "Ref types"
      [(is (= (set (eval-roam-attr-query []
                                         graph/status-attr
                                         [[(get-operator :=)
                                           [[graph/in-progress ref-type]]]]
                                         [graph/in-progress]))
              #{graph/task-1}))
       (is (= (set (eval-roam-attr-query []
                                         graph/status-attr
                                         [[(get-operator :=)
                                           [[graph/completed ref-type]]]]
                                         [graph/completed]))
              #{}))])
    (testing "Date ref types"
      [(is (= (set (eval-roam-attr-query []
                                         graph/deadline-attr
                                         [[(get-operator :is_dnp)
                                           []]]
                                         nil))
              #{graph/task-1 graph/task-2}))
       (is (= (set (eval-roam-attr-query []
                                         graph/deadline-attr
                                         [[(get-operator :<)
                                           [[graph/nov-1-2021 ref-type]]]]
                                         nil))
              #{graph/task-1 graph/task-2}))
       (is (= (set (eval-roam-attr-query []
                                         graph/deadline-attr
                                         [[(get-operator :>)
                                           [[graph/nov-1-2021 ref-type]]]]
                                         nil))
              #{}))
       (is (= (set (eval-roam-attr-query []
                                         graph/deadline-attr
                                         [[(get-operator :>)
                                           [[graph/october-26-2021 ref-type]]]]
                                         nil))
              #{graph/task-1}))
       (is (= (set (eval-roam-attr-query []
                                         graph/deadline-attr
                                         [[(get-operator :<=)
                                           [[graph/october-26-2021 ref-type]]]]
                                         nil))
              #{graph/task-2}))
       (is (= (set (eval-roam-attr-query []
                                         graph/deadline-attr
                                         [[(get-operator :>=)
                                           [[graph/october-26-2021 ref-type]]]]
                                         nil))
              #{graph/task-1 graph/task-2}))])))
