(ns alumbra.canonical.value)

(declare resolve-value)

;; ## Input Type

(defn- resolve-input-type
  [opts {:keys [type-name]} {:keys [alumbra/value-type
                                    alumbra/object]}]
  (when (not= value-type :null)
    (let [fields (get-in opts [:schema :input-types type-name :fields])]
      (->> (for [{:keys [alumbra/field-name
                         alumbra/value]} object
                 :let [{:keys [type-description]} (get fields field-name)]
                 :when type-description]
             [field-name (resolve-value opts type-description value)])
           (into {})))))

;; ## List

(defn- resolve-list-type
  [opts {:keys [type-description]} {:keys [alumbra/value-type
                                           alumbra/list]}]
  (when (not= value-type :null)
    (mapv #(resolve-value opts type-description %) list)))

;; ## Scalar

(defn- resolve-scalar
  [opts type-description {:keys [alumbra/value-type] :as value}]
  (merge
    (select-keys type-description [:type-name :non-null?])
    {:value (case value-type
              :string   (:alumbra/string value)
              :integer  (:alumbra/integer value)
              :float    (:alumbra/float value)
              :boolean  (:alumbra/boolean value)
              :enum     (:alumbra/enum value)
              :null     nil)}))

;; ## Named Type

(defn- resolve-named-type
  [{:keys [schema] :as opts} {:keys [type-name] :as type} value]
  (let [kind (get-in schema [:type->kind type-name])]
    (case kind
      (:scalar :enum) (resolve-scalar opts type value)
      :input-type     (resolve-input-type opts type value))))

;; ## Variable

(defn- resolve-variable
  [{:keys [variables]} {:keys [alumbra/variable-name]}]
  (if (contains? variables variable-name)
    (get variables variable-name)
    (throw
      (IllegalStateException.
        (str "variable missing: $" variable-name)))))

;; ## Resolve Value

(defn resolve-value
  [opts
   {:keys [type-name] :as type-description}
   {:keys [alumbra/value-type] :as value}]
  (cond (= value-type :variable)
        (resolve-variable opts value)

        type-name
        (resolve-named-type opts type-description value)

        :else
        (resolve-list-type opts type-description value)))
