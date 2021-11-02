(ns query.core
  (:require [clojure.string :as str]
            [clojure.set]
            [roam.datascript :as rd]
            [query.errors :refer [throw-error generic-query-error]]
            [query.util :refer [branch?]]
            [query.attr.core :refer [attr-query? attr-query]]
            [query.roam-native :refer [roam-native-query? roam-native-query]]))

(defn- execute-generic-query-clause [blocks clause-block]
  (let [block-string (str/trim (clause-block :block/string))
        str-lower (str/lower-case block-string)
        children (->> (clause-block :block/children)
                      (sort-by :block/order))]
    (cond (branch? str-lower)
          (cond (= str-lower "and")
                (reduce #(let [result (execute-generic-query-clause %1 %2)]
                           (if (seq result)
                             result
                             [])) blocks children)

                (= str-lower "or")
                (vec (reduce #(clojure.set/union (set %1)
                                                 (set (execute-generic-query-clause blocks %2))) [] children))

                (= str-lower "not")
                (if (seq blocks)
                  (vec (clojure.set/difference blocks (reduce #(execute-generic-query-clause %1 %2) blocks children)))
                  ; Don't allow people to run a NOT clause on their whole database, too risky
                  [])

                :else (throw-error generic-query-error str-lower))
          (attr-query? clause-block)
          (attr-query blocks clause-block children)

          (roam-native-query? block-string)
          (roam-native-query blocks block-string)


          :else (throw-error generic-query-error block-string))))

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

;; (defn- get-all-blocks []
;;   (rd/q '[:find [?e ...]
;;           :in $ ?nonce
;;           :where
;;           (or [?e :block/string]
;;               [?e :node/title])]
;;         0))

(defn generic-roam-query [query-uid]
  (let [query-tree (ffirst (get-block-children query-uid))]
    (if query-tree
      (execute-generic-query-clause [] query-tree)
      [])))

