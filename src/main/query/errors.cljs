(ns query.errors)

(defonce roam-native-error "#ROAM_NATIVE!")
(defonce generic-query-error "#GENERIC_QUERY!")

(defonce errors {roam-native-error "Roam native parsing error!"})

(defn- get-error [name]
  (or (get errors name) "Unknown error"))

(defn throw-error [name data]
  (let [description (get-error name)]
    (throw (ex-info description
                    {:error name
                     :description description
                     :expr data}))))
