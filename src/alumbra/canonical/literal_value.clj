(ns alumbra.canonical.literal-value)

(declare resolve-literal-value)

;; ## Assert

(defn- type-shorthand
  [{:keys [type-name non-null? type-description]}]
  (str
    (or type-name
        (str "[" (type-shorthand type-description) "]"))
    (if non-null?  "!")))

(defmacro ^:private assert-type
  [predicate expected-type value]
  `(let [v# ~value
         t# ~expected-type]
     (if (nil? v#)
       (when (:non-null? t#)
         (throw
           (IllegalArgumentException.
             (format
               "null value not allowed for expected type '%s'."
               (type-shorthand t#)))))
       (when-not ~predicate
         (throw
           (IllegalArgumentException.
             (format
               "value does not match expected type '%s': %s"
               (type-shorthand t#)
               v#)))))))

;; ## Translation to Canonical Format

(defn- resolve-scalar
  [opts {:keys [type-name non-null?] :as type} value]
  (assert-type (not (or (map? value) (sequential? value))) type value)
  {:type-name type-name
   :non-null? non-null?
   :value     value})

(defn- resolve-enum
  [{:keys [schema]} {:keys [type-name non-null?] :as type} value]
  (let [{:keys [enum-values]} (get-in schema [:enums type-name])
        value (name value)]
    (assert-type
      (and enum-values (contains? enum-values value))
      type
      value))
  {:type-name type-name
   :non-null? non-null?
   :value     value})

(defn- resolve-input-type
  [{:keys [schema] :as opts}
   {:keys [type-name non-null?] :as type}
   value]
  (assert-type (map? value) type value)
  (when value
    (let [fields (get-in schema [:input-types type-name :fields])]
      (->> (for [[field-name field-value] value
                 :let [{:keys [type-description]} (get fields field-name)]
                 :when type-description]
             [field-name
              (resolve-literal-value opts type-description field-value)])
           (into {})))))

(defn- resolve-named-type
  [{:keys [schema] :as opts}
   {:keys [type-name] :as type}
   value]
  (let [kind (get-in schema [:type->kind type-name])]
    (case kind
      :scalar     (resolve-scalar opts type value)
      :enum       (resolve-enum opts type value)
      :input-type (resolve-input-type opts type value))))

(defn- resolve-list-type
  [opts {:keys [non-null? type-description] :as list-type} value]
  (assert-type (sequential? value) list-type value)
  (some->> value
           (mapv #(resolve-literal-value opts type-description %))))

(defn resolve-literal-value
  [opts {:keys [type-name non-null?] :as type} value]
  (if type-name
    (resolve-named-type opts type value)
    (resolve-list-type opts type value)))
