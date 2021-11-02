(ns query.errors)

(defonce roam-native-error "#ROAM_NATIVE!")
(defonce generic-query-error "#GENERIC_QUERY!")

(defonce errors {roam-native-error "Roam native parsing error!"})

(defn- get-error [name]
  (or (get errors name) "Unknown error"))

(defn get-error-name [error]
  (or (:error (ex-data error)) "#ERROR!"))

(defn throw-error [name data]
  (let [description (get-error name)]
    (throw (ex-info description
                    {:error name
                     :description description
                     :expr data}))))

(defn log-error [id e]
  (let [{:keys [error description expr]} (ex-data e)]
    (js/console.warn
     (str "Error: \n"
          error ": " (or description "Unknown error")
          " at " id "\n"
          "Expression: " expr "\n"))))