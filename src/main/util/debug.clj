(ns util.debug)

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

; TODO: This doesn't work well with Clojurescript + source maps + debugging
;; (defmacro debug [form]
;;   (list 'do
;;         (list 'js-debugger)
;;         form))

; TODO: I have to import tufte in my root file because this is dynamically generating code.
; But can't I "resolve" those tufte functions here??? I tried and can't because
; "You can't take the value of a macro"
; This doesn't even work anyway - repeat > 1 doesn't properly "combine" the same functions and idk why
(defmacro perf [settings & forms]
  (let [functions (for [func forms]
                    ; TODO append func name with actual index instead of rand-int
                    `(taoensso.tufte/p (keyword (str (name (first '~func)) "-" (rand-int 100000))) ~func))
        settings (if (empty? settings)
                   ; Default settings
                   ;; repeat is a custom property to repeat all top lvl functions
                   {:repeat 10}
                   settings)]
    `(do (js/console.log "Profiling...")
         (let [[_# pstats#] (taoensso.tufte/profiled
                             ~settings
                             (dotimes [_# (:repeat ~settings)]
                               ~@functions))]
           (js/console.log "%cResults:", "color: green; font-size: 20px; font-weight: bold;")
           (js/console.log (taoensso.tufte/format-pstats pstats# {:columns [:n-calls :min :p50 :p90 :p99 :max :mean :mad :clock :total]
                                                                  :sort-fn (fn [m#] (get m# :sum))}))))))