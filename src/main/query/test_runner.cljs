(ns query.test-runner
  (:require [cljs.test :refer-macros [run-all-tests]]
            [roam.datascript :as rd]
            [cljs.nodejs]))

#_:clj-kondo/ignore
(defn main []
  ; This makes cljs-http work in Node (where I run tests)
  (set! js/XMLHttpRequest (cljs.nodejs/require "xhr2"))
  (-> (rd/init-db+)
      (.then #(run-all-tests #".*-spec$"))
      (.catch #(js/console.error "Error initializing DB: " %))))
