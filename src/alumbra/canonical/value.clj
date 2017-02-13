(ns alumbra.canonical.value)

;; ## Helpers

(defn- resolve-object
  [continue-fn opts {:keys [type-name]} {:keys [alumbra/object]}]
  (let [fields (get-in opts [:schema :input-types type-name :fields])]
    (->> (for [{:keys [alumbra/field-name
                       alumbra/value]} object
               :let [field-type (get fields field-name)]]
           [field-name (continue-fn opts field-type value)])
         (into {}))))

(defn- resolve-list
  [continue-fn opts argument-type {:keys [alumbra/list]}]
  (mapv #(continue-fn opts argument-type %) list))

(defn- resolve-variable
  [{:keys [variables]} {:keys [alumbra/variable-name]}]
  (if (contains? variables variable-name)
    (get variables variable-name)
    (throw
      (IllegalStateException.
        (str "variable missing: $" variable-name)))))

;; ## Scalar

(defn- resolve-scalar
  [opts argument-type {:keys [alumbra/value-type] :as value}]
  (merge
    (select-keys argument-type [:type-name :non-null?])
    {:value (case value-type
              :string   (:alumbra/string value)
              :integer  (:alumbra/integer value)
              :float    (:alumbra/float value)
              :boolean  (:alumbra/boolean value)
              :enum     (:alumbra/enum value)
              :null     nil)}))

;; ## Resolve Value

(defn resolve-value
  [opts argument-type {:keys [alumbra/value-type] :as value}]
  (case value-type
    :list     (resolve-list resolve-value opts argument-type value)
    :object   (resolve-object resolve-value opts argument-type value)
    :variable (resolve-variable opts value)
    (resolve-scalar opts argument-type value)))
