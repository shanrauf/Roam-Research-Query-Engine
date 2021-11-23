(ns query.attr.attr-spec
  (:require [cljs.test :refer (deftest is testing)]
            [roam.test-graph :as graph]
            [query.attr.core :refer [eval-roam-attr-query
                                     eval-reverse-roam-attr-query
                                     eval-datomic-attr-query]]
            [query.attr.operation :refer [get-operator]]
            [query.attr.value :refer [text-type num-type ref-type extract-attr-values]]))


(deftest roam-attr-spec []
  (testing "Attribute value parsing:"
    (testing "Ref type"
      [(is (= (extract-attr-values "[[Status]]" graph/type-attr [graph/status-attr])
              [[graph/status-attr ref-type]]))
       (is (= (extract-attr-values "#Status" graph/type-attr [graph/status-attr])
              [[graph/status-attr ref-type]]))
       (is (= (extract-attr-values "#[[Status]]" graph/type-attr [graph/status-attr])
              [[graph/status-attr ref-type]]))]))
  (testing "Reverse Roam attributes"
    [(is (= (set (eval-reverse-roam-attr-query [] graph/type-attr graph/task-3))
            #{graph/task-type}))
     (is (= (set (eval-reverse-roam-attr-query [] graph/todos-attr graph/task-1))
            #{graph/task-2 graph/task-3}))
     (is (= (set (eval-reverse-roam-attr-query [] graph/status-attr graph/task-2))
            #{graph/completed graph/october-28-2021}))])
  (testing "Datomic attributes"
    [(is (= (set (eval-datomic-attr-query []
                                          :block/children
                                          [[(get-operator :includes)
                                            (mapv #(identity [% ref-type])
                                                  (:block/children graph/test-block-0))]]
                                          (:block/children graph/test-block-0)))
            (set [(:db/id graph/test-block-0)])))
     (is (= (set (eval-datomic-attr-query []
                                          :block/refs
                                          [[(get-operator :includes)
                                            [[graph/october-31-2021 ref-type]]]]
                                          [graph/october-31-2021]))
            (set [graph/task-2])))
     (is (= (set (eval-datomic-attr-query []
                                          :create/time
                                          [[(get-operator :=)
                                            [[1635382157454 num-type]]]]
                                          []))
            (set [graph/task-1])))
     (is (= (set (eval-datomic-attr-query []
                                          :block/uid
                                          [[(get-operator :=)
                                            [["w9YC_4vTm" text-type]]]]
                                          []))
            (set [graph/task-1])))])
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
                                           [["/task 2$/" text-type]]]]
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
              #{}))
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
