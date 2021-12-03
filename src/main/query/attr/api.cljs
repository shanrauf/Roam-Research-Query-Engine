(ns query.attr.api
  (:require [clojure.set :as set]
            [query.attr.core :refer [eval-roam-attr-query]]))

(defn- preserve-blocks-with-empty-attr-vals
  "Datalog queries filter out blocks that don't have corresponding attr
   values, so this manually adds those back into the result.
   Profiling tests: This function slows adds 3% overhead.

   TODO: Is there a way to have Datomic do this for me? i.e. if no attr values,
   set a default value of []"
  [all-blocks block-val-pairs]
  (into block-val-pairs
        (map #(identity [% []]) (set/difference (set all-blocks)
                                                (set (map first block-val-pairs))))))

(defn- block-vec->map
  "[1 2]
   →
   {:1 {}
    :2 {}}"
  [blocks]
  (if (empty? blocks)
    {}
    (reduce #(assoc %1 (keyword (str %2)) {}) {} blocks)))

(defn- assoc-attr-vals [block-map block-vals-pair attr]
  (let [[block attr-vals] block-vals-pair]
    (assoc-in block-map
              [(keyword (str block))
               (keyword (str attr))]
              attr-vals)))

(defn- get-values-for-roam-attr
  "Returns values for a single attribute on blocks.
  (searches whole DB if blocks empty)"
  [blocks attr]
  (->> (eval-roam-attr-query blocks attr)
       (preserve-blocks-with-empty-attr-vals blocks)))

(defn get-roam-attr-values
  "Returns values for every attribute on blocks.
   (searches whole DB if blocks empty)"
  [blocks attrs]
  (let [block-map (block-vec->map blocks)]
    (reduce #(let [block-vals-pairs (get-values-for-roam-attr blocks %2)]
               (reduce (fn [m block-vals-pair]
                         (assoc-attr-vals m block-vals-pair %2)) %1 block-vals-pairs)) block-map attrs)))
