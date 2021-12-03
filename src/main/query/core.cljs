(ns query.core
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [roam.datascript :as rd]
            [query.util :refer [is_dnp generic-query-clause? get-query-uid branch? add-current-blocks-to-query]]
            [query.attr.core :refer [attr-query? attr-query]]
            [query.attr.value :refer [parse-attribute-ref-value
                                      ref-type
                                      dnp-title-regex
                                      attr-value->type]]
            [query.roam-native :refer [roam-native-query? roam-native-query]]))

(declare eval-generic-query-clause)

(defn- stop-on-failure [query-result]
  (if (seq query-result)
    query-result
    []))

(defn- get-children [uid]
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

(defn- eval-generic-roam-query [query-uid blocks]
  (let [query-tree (->> (get-children query-uid)
                        (map first))]
    (if query-tree
      (eval-generic-query-clause blocks (add-implicit-and-clause query-tree))
      [])))

(defn- dnp-filter-clause? [block-string]
  (= (->
      (str/trim block-string)
      (str/lower-case)
      (keyword))
     is_dnp))

(defn- eval-dnp-clause [blocks]
  (rd/q (add-current-blocks-to-query
         '[:find [?block ...]
           :in $ ?dnp-title-regex
           :where
           [?block :node/title ?title]
           [(re-matches ?dnp-title-regex ?title)]]
         blocks)
        dnp-title-regex blocks))


; NOTE: Roam code blocks break when there are backticks, and I can't escape them
(defn- javascript-clause? [block-string]
  (str/includes? block-string "``javascript"))
(defn- execute-javascript-clause [block-string]
  (let [script (-> block-string
                   (subs 13 (- (count block-string) 3)))]
    (vec (-> (js/Function. script)
             (. call)))))

(defn- ref-list-clause? [block]
  (let [block-string (:block/string block)]
    (= ref-type
       (-> (parse-attribute-ref-value block-string
                                      block-string
                                      (:block/refs block))
           (first)
           (attr-value->type)))))
(defn- eval-ref-list-clause [block]
  (mapv :db/id (:block/refs block)))


(defn- eval-generic-query-clause [blocks clause-block]
  (let [block-string (str/trim (clause-block :block/string))
        str-lower (str/lower-case block-string)

        children (->> (clause-block :block/children)
                      (sort-by :block/order))]
    ; NOTE: The order of conditions matters a lot
    ; e.g. a nested query clause `query::` could be read
    ; as an attribute clause `query:: input_value`.
    ; (shifting conditions around will break tests though so it's obvious)
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

          (dnp-filter-clause? block-string)
          (eval-dnp-clause blocks)

          (generic-query-clause? clause-block)
          (eval-generic-roam-query (get-query-uid clause-block) blocks)

          (attr-query? clause-block)
          (attr-query blocks clause-block eval-generic-roam-query)

          (roam-native-query? block-string)
          (roam-native-query blocks block-string)

          (javascript-clause? block-string)
          (execute-javascript-clause block-string)

          (ref-list-clause? clause-block)
          (eval-ref-list-clause clause-block)

          :else (throw (js/Error. (str "Unknown query clause: " block-string))))))

(defn generic-roam-query [query-uid]
  (eval-generic-roam-query query-uid []))

#_:clj-kondo/ignore
(defn ^:dev/after-load start []
  (js/console.log "Reloaded:"))

#_:clj-kondo/ignore
(defn ^:dev/before-load stop []
  (js/console.log "Reloading..."))

#_:clj-kondo/ignore
(defn init []
  (js/console.log "Initializing DB... (avoid hot reloading or running functions that rely on it for a few seconds)")
  (-> (rd/init-db+)
      (.then #(js/console.log "Generic roam queries"))
      (.catch #(js/console.error "Error initializing DB: " %))))
