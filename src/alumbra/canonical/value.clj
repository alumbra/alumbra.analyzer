(ns alumbra.canonical.value)

;; ## Helpers

(defn- resolve-object
  [continue-fn opts {:keys [alumbra/object]}]
  (->> (for [{:keys [alumbra/field-name
                     alumbra/value]} object]
         [field-name (continue-fn opts value)])
       (into {})))

(defn- resolve-list
  [continue-fn opts {:keys [alumbra/list]}]
  (mapv #(continue-fn opts %) list))

(defn- resolve-variable
  [{:keys [variables]} {:keys [alumbra/variable-name]}]
  (cond (contains? variables variable-name)
        (get variables variable-name)

        ;; TODO: check default value

        :else
        (throw
          (IllegalStateException.
            (str "variable missing: $" variable-name)))))

;; ## Resolve Value

(defn resolve-value
  [opts {:keys [alumbra/value-type] :as value}]
  (case value-type
    :string   (:alumbra/string value)
    :integer  (:alumbra/integer value)
    :float    (:alumbra/float value)
    :boolean  (:alumbra/boolean value)
    :variable (resolve-variable opts value)
    :object   (resolve-object resolve-value opts value)
    :enum     (keyword (:alumbra/enum value))
    :null     nil
    :id       (:alumbra/id value)
    :list     (resolve-list resolve-value opts value)))
