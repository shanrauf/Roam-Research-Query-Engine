(ns query.core
  (:require [clojure.string :as str]
            [clojure.set]
            [roam.datascript :as rd]
            [query.errors :refer [throw-error generic-query-error]]
            [query.util :refer [branch?]]
            [query.attr.core :refer [attr-query? attr-query]]
            [query.roam-native :refer [roam-native-query? roam-native-query]]))

(defn execute-generic-query-clause [blocks clause-block]
  (let [block-string (str/trim (clause-block :block/string))
        str-lower (str/lower-case block-string)
        children (clause-block :block/children)]
    (cond (branch? str-lower)
          (cond (= str-lower "and")
                (reduce #(execute-generic-query-clause %1 %2) blocks children)

                (= str-lower "or")
                (reduce #(clojure.set/union (set %1) (set (execute-generic-query-clause blocks %2))) blocks children)

                (= str-lower "not")
                (clojure.set/difference blocks (reduce #(execute-generic-query-clause %1 %2) blocks children))

                :else (throw-error generic-query-error str-lower))
          (attr-query? block-string)
          (attr-query blocks clause-block children)

          (roam-native-query? block-string)
          (roam-native-query blocks block-string))))

(defn- get-block-children [uid]
  (nth (rd/q '[:find (pull ?children [:db/id :block/order :block/uid :node/title :block/string {:block/refs 2} {:block/children ...}])
               :in $ ?query-uid
               :where
               [?query :block/uid ?query-uid]
               [?query :block/children ?children]]
             uid)
       0))

(defn- get-all-blocks []
  (rd/q '[:find ?e
          :in $ ?nonce
          :where
          (or [?e :block/string]
              [?e :node/title])]
        0))

(defn generic-roam-query [query-uid]
  (let [query-tree (first (get-block-children query-uid))]
    (execute-generic-query-clause [] query-tree)))

