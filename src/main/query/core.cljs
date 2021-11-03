(ns query.core
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [roam.datascript :as rd]
            [query.errors :refer [throw-error generic-query-error]]
            [query.util :refer [branch? ref->ref-content]]
            [query.attr.core :refer [attr-query? attr-query]]
            [query.attr.value :refer [parse-attribute-ref-value
                                      ref-type
                                      attr-value->type]]
            [query.roam-native :refer [roam-native-query? roam-native-query]]))

(declare execute-generic-query-clause)

(defn- stop-on-failure [query-result]
  (if (seq query-result)
    query-result
    []))

(defn- generic-query-ref? [clause-block]
  (let [ref-count (count (:block/refs clause-block))
        ref (-> (:block/string clause-block)
                (str/trim)
                (ref->ref-content))]
    (if (and (= ref-count 1)
             (seq (rd/q '[:find ?e
                          :in $ ?ref
                          :where
                          [?e :block/uid ?ref]
                          [?e :block/parents ?direct-parent]
                          [?direct-parent :block/children ?e]
                          [?query-attr :node/title "query"]
                          [?direct-parent :attrs/lookup ?query-attr]]
                        ref)))
      true
      false)))

(defn- get-block-children [uid]
  (let [result (rd/q '[:find (pull ?children [:db/id :block/order :block/uid :node/title :block/string {:block/refs 2} {:block/children ...}])
                       :in $ ?query-uid
                       :where
                       [?query :block/uid ?query-uid]
                       [?query :block/children ?children]]
                     uid)]
    (if (seq result)
      result
      [])))

(defn- add-implicit-and-clause [query-tree]
  {:block/string "AND"
   :block/children query-tree})

(defn- execute-generic-and-clause [blocks children]
  (vec (reduce #(stop-on-failure (execute-generic-query-clause %1 %2))
               blocks
               children)))

; TODO: Roam code blocks break when there are backticks, and I can't escape them
; TODO: how do you test this outside of Roam (because here "window" is not defined)
(defn- javascript-clause? [block-string]
  (str/includes? block-string "``javascript"))
(defn- execute-javascript-clause [block-string]
  (let [script (-> block-string
                   (subs 13 (- (count block-string) 3)))]
    (println script)
    (if script
      (vec (-> (js/Function. script)
               (. call)))
      [])))

(defn- ref-list? [block-string]
  (= ref-type
     (attr-value->type (parse-attribute-ref-value block-string block-string nil))))

(defn- execute-ref-list-clause [block]
  (mapv :db/id (:block/refs block)))

(defn- execute-generic-roam-query [query-uid blocks]
  (let [query-tree (->> (get-block-children query-uid)
                        (map first))]
    (if query-tree
      (execute-generic-query-clause blocks (add-implicit-and-clause query-tree))
      [])))


(defn- execute-generic-query-clause [blocks clause-block]
  (let [block-string (str/trim (clause-block :block/string))
        str-lower (str/lower-case block-string)

        children (->> (clause-block :block/children)
                      (sort-by :block/order))]
    (cond (branch? str-lower)
          (cond (= str-lower "and")
                (execute-generic-and-clause blocks children)

                (= str-lower "or")
                (vec (reduce #(set/union (set %1)
                                         (set (execute-generic-query-clause blocks %2))) [] children))

                (= str-lower "not")
                (if (seq blocks)
                  (vec (set/difference (set blocks)
                                       (set (execute-generic-and-clause blocks children))))
                  [])

                :else (throw-error generic-query-error str-lower))
          (attr-query? clause-block)
          (attr-query blocks clause-block children)

          (roam-native-query? block-string)
          (roam-native-query blocks block-string)

          (javascript-clause? block-string)
          (execute-javascript-clause block-string)

          (generic-query-ref? clause-block)
          (let [query-uid (-> (:block/string clause-block)
                              (str/trim)
                              (ref->ref-content))]
            (execute-generic-roam-query query-uid blocks))

          (ref-list? block-string)
          (execute-ref-list-clause clause-block)

          :else (throw-error generic-query-error block-string))))

(defn generic-roam-query [query-uid]
  (execute-generic-roam-query query-uid []))

