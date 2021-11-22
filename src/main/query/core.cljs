(ns query.core
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [roam.datascript :as rd]
            [query.util :refer [branch? ref->ref-content]]
            [query.attr.core :refer [attr-query? attr-query]]
            [query.attr.value :refer [parse-attribute-ref-value
                                      ref-type
                                      attr-value->type]]
            [query.roam-native :refer [roam-native-query? roam-native-query]]))

(declare eval-generic-query-clause)

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

(defn- uid->block-children [uid]
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

(defn- eval-generic-and-clause [blocks children]
  (vec (reduce #(stop-on-failure (eval-generic-query-clause %1 %2))
               blocks
               children)))

; TODO: Roam code blocks break when there are backticks, and I can't escape them
(defn- javascript-clause? [block-string]
  (str/includes? block-string "``javascript"))
(defn- execute-javascript-clause [block-string]
  (let [script (-> block-string
                   (subs 13 (- (count block-string) 3)))]
    (vec (-> (js/Function. script)
             (. call)))))

(defn- ref-list-clause? [block-string]
  (= ref-type
     (attr-value->type (parse-attribute-ref-value block-string block-string nil))))
(defn- eval-ref-list-clause [block]
  (mapv :db/id (:block/refs block)))

(defn- eval-generic-roam-query [query-uid blocks]
  (let [query-tree (->> (uid->block-children query-uid)
                        (map first))]
    (if query-tree
      (eval-generic-query-clause blocks (add-implicit-and-clause query-tree))
      [])))

(defn- eval-generic-query-clause [blocks clause-block]
  (let [block-string (str/trim (clause-block :block/string))
        str-lower (str/lower-case block-string)

        children (->> (clause-block :block/children)
                      (sort-by :block/order))]
    (cond (branch? str-lower)
          (cond (= str-lower "and")
                (eval-generic-and-clause blocks children)

                (= str-lower "or")
                (vec (reduce #(set/union (set %1)
                                         (set (eval-generic-query-clause blocks %2))) [] children))

                (= str-lower "not")
                (if (seq blocks)
                  (vec (set/difference (set blocks)
                                       (set (eval-generic-and-clause blocks children))))
                  [])

                :else (throw (js/Error. (str "Invalid branch: " str-lower))))
          (attr-query? clause-block)
          (attr-query blocks clause-block)

          (roam-native-query? block-string)
          (roam-native-query blocks block-string)

          (javascript-clause? block-string)
          (execute-javascript-clause block-string)

          (generic-query-ref? clause-block)
          (let [query-uid (-> (:block/string clause-block)
                              (str/trim)
                              (ref->ref-content))]
            (eval-generic-roam-query query-uid blocks))

          (ref-list-clause? block-string)
          (eval-ref-list-clause clause-block)

          :else (throw (js/Error. (str "Unknown query clause: " block-string))))))

(defn generic-roam-query [query-uid]
  (eval-generic-roam-query query-uid []))

(defn init []
  (println "Generic roam queries"))

