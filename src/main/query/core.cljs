(ns query.core
  (:require [clojure.string :as str]
            [roam.datascript :as rd]
            [query.roam-native :refer [roam-native-rule]]
            [query.util :refer [branch?]]))

(defonce base-query {:find []
                     :in []
                     :where []})

(defonce query-rules ['[(ref-to-eid ?ref ?e)
                        [(str ?ref) ?ref-str]
                        [(count ?ref-str) ?len]
                        [(- ?len 2) ?end]
                        [(subs ?ref-str 2 ?end) ?str]
                        (or (and [(clojure.string/starts-with? ?ref-str "[[")]

                                 [?e :node/title ?str])
                            (and [(clojure.string/starts-with? ?ref-str "((")]

                                 [?e :block/uid ?str]))]
                      roam-native-rule])

;; (defn parse-generic-query-clause [block, current-branch]
;;   (let [block-string (str/trim (block :block/string))
;;         str-lower (str/lower-case block-string)
;;         children (block :block/children)]
;;     (cond
;;       (branch? str-lower)
;;       (let [child-clauses (mapv #(parse-generic-query-clause % str-lower) children)]
;;         (branch->datalog current-branch str-lower child-clauses)))))

;; (defn- get-block-children [uid]
;;   (nth (rd/q '[:find (pull ?children [:db/id :block/uid :node/title :block/string {:block/refs 2} {:block/children ...}])
;;                :in $ ?query-uid
;;                :where
;;                [?query :block/uid ?query-uid]
;;                [?query :block/children ?children]]
;;              uid)
;;        0))

;; (defn generic-roam-query [query-uid]
;;   (let [block-children (get-block-children query-uid)
;;         query-clauses (mapv #(parse-generic-query-clause % "and") block-children)]
;;     (assoc base-query :where (concat (base-query :where) query-clauses))))

