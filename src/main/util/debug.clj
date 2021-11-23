(ns util.debug
  (:require [taoensso.tufte :as tufte]))

(defmacro defntraced
  [sym & body]
  (let [[_ _ [_ & specs]] (macroexpand `(defn ~sym ~@body))
        new-specs
        (map
         (fn [[args body]]
           (let [prns (for [arg args]
                        `(js/console.log (str '~arg) "=" (pr-str ~arg)))]
             (list
              args
              `(do
                 (js/console.groupCollapsed (str '~sym " " '~args))
                 ~@prns
                 (let [res# ~body]
                   (js/console.log "=>" (pr-str res#))
                   (js/console.groupEnd)
                   res#)))))
         specs)]
    `(def ~sym (fn ~@new-specs))))

(defmacro log [form]
  (if (list? form)
    (let [prns (map-indexed
                (fn [idx arg]
                  `(if (= (type '~arg) cljs.core/Symbol)
                     (js/console.log (str '~arg) "=" (pr-str ~arg))
                     (js/console.log (str "Argument #" ~idx) "=" (pr-str ~arg))))
                (rest form))]
      `(do (js/console.groupCollapsed (str '~form ":") (pr-str ~form))
           (js/console.log "Arguments:")
           ~@prns
           (js/console.log "=> " (pr-str ~form))
           (js/console.groupEnd)
           ~form))
    `(do (js/console.log ~form)
         ~form)))

; This doesn't work well with Clojurescript + source maps + debugging
;; (defmacro debug [form]
;;   (list 'do
;;         (list 'js-debugger)
;;         form))

; NOTE: I have to import tufte in my root file because this is dynamically generating code.
;; But can't I "resolve" those tufte functions here???
;;; idrc though, I often have to import the library anyway for adding defnp/p in my codebase
(defmacro perf [& forms]
  ;; (refer 'taoensso.tufte :only '[p profiled format-pstats])
  (let [first-form (first forms)
        functions (for [func (rest forms)]
                    ; TODO append func name with actual index instead of rand-int
                    `(taoensso.tufte/p (keyword (str (name (first '~func)) "-" (rand-int 100000))) ~func))
        settings (if (empty? first-form)
                   ; Default settings
                   ;; repeat is a custom property to repeat all top lvl functions
                   {:repeat 1}
                   first-form)]
    `(do (js/console.log "Profiling...")
         (let [[_# pstats#] (taoensso.tufte/profiled
                             ~settings
                             (dotimes [_# (or (:repeat ~settings) 1)]
                               ~@functions))]
           (js/console.log "%cProfiling results:", "color: green; font-weight: bold;")
           (js/console.log (taoensso.tufte/format-pstats pstats# {:columns [:n-calls :min :p50 :p90 :p99 :max :mean :mad :clock :total]
                                                                  :sort-fn (fn [m#] (get m# :sum))}))))))