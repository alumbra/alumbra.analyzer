(ns alumbra.analyzer.value)

;; ## Helpers

(defn- read-object
  [continue-fn {:keys [alumbra/object]}]
  (->> (for [{:keys [alumbra/field-name
                     alumbra/value]} object]
         [field-name (continue-fn value)])
       (into {})))

(defn- read-list
  [continue-fn {:keys [alumbra/list]}]
  (mapv continue-fn list))

;; ## Resolve Value

(defn read-value
  [{:keys [alumbra/value-type] :as value}]
  (case value-type
    :string   (:alumbra/string value)
    :integer  (:alumbra/integer value)
    :float    (:alumbra/float value)
    :boolean  (:alumbra/boolean value)
    :object   (read-object read-value value)
    :enum     (keyword (:alumbra/enum value))
    :null     nil
    :id       (:alumbra/id value)
    :list     (read-list read-value value)))
