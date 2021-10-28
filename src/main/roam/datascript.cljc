(ns roam.datascript
  (:require [datascript.core :as d]
            [roam.test-graph]))

(defonce schema (roam.test-graph/roam-graph :schema))
(defonce datoms (reduce #(conj %1 (apply d/datom %2)) #{} (roam.test-graph/roam-graph :datoms)))
(defonce db (d/conn-from-datoms datoms schema))

(defn- add-db-to-args [args]
  (if (> (count args) 1)
    (into [(first args) @db] (rest args))
    (conj args @db)))

(defn q [& args]
  (apply d/q (add-db-to-args args)))

(defn init []
  (println (q '[:find [?block ...]
                :in $ ?input-refs %
                :where
                (roam-native ?block ?input-refs)]
              [49, 50]
              '[[(roam-native ?block ?and-refs)
                 [(identity ?and-refs) [?ref0 ...]]
                 [?block :block/refs ?ref0]
                 (not-join [?block ?and-refs]
                           [(identity ?and-refs) [?ref ...]]
                           (not-join [?block ?ref]
                                     (or-join [?block ?ref]
                                              [?block :block/refs ?ref]
                                              (and [?block :block/parents ?parents]
                                                   [?parents :block/refs ?ref])
                                              [?block :block/page ?ref])))]]))
  true)

(defn new-improved []
  (println (q '[:find [?block ...]
                :in $ ?and-refs ?not-refs %
                :where
                (roam-native ?block ?and-refs ?not-refs)]
              [49] [50]
              '[[(roam-native ?block ?and-refs ?not-refs)
                 [(identity ?and-refs) [?ref0 ...]]
                 [?block :block/refs ?ref0]
                 (not-join [?block ?and-refs]
                           [(identity ?and-refs) [?ref ...]]
                           (not-join [?block ?ref]
                                     (or-join [?block ?ref]
                                              [?block :block/refs ?ref]
                                              (and [?block :block/parents ?parents]
                                                   [?parents :block/refs ?ref])
                                              [?block :block/page ?ref])))
                ; Filter out not-refs
                 (not
                  (not-join [?block ?not-refs]
                            [(identity ?not-refs) [?ref ...]]
                            (not-join [?block ?ref]
                                      (or-join [?block ?ref]
                                               [?block :block/refs ?ref]
                                               (and [?block :block/parents ?parents]
                                                    [?parents :block/refs ?ref])
                                               [?block :block/page ?ref]))))]]))
  true)

(defn new-improved []
  (println (q '[:find [?block ...]
                :in $ ?and-refs ?not-refs %
                :where
                ; Set ?block to the subset of blocks you want to search
                ;; Optimization: Only search blocks that contain the AND refs you care about
                [(identity ?and-refs) [?ref0 ...]]
                [?block :block/refs ?ref0]
                (roam-native ?block ?and-refs)
                (not (roam-native ?block ?not-refs))]
              [49, 50] [125]
              '[[(roam-native ?block ?refs)
                 ; Double negation: https://stackoverflow.com/questions/43784258/find-entities-whose-ref-to-many-attribute-contains-all-elements-of-input
                 (not-join [?block ?refs]
                           [(identity ?refs) [?ref ...]]
                           (not-join [?block ?ref]
                                     (or-join [?block ?ref]
                                              [?block :block/refs ?ref]
                                              (and [?block :block/parents ?parents]
                                                   [?parents :block/refs ?ref])
                                              [?block :block/page ?ref])))]]))
  true)