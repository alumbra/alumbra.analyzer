(ns alumbra.canonical.variables
  (:require [alumbra.canonical
             [types :refer [as-type-description]]
             [value :refer [resolve-value]]]))

(declare resolve-variable-value)

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
     (assert (or (and (nil? v#) (not (:non-null? t#)))
                 ~predicate)
             (format
               "value does not match expected type '%s': %s"
               (type-shorthand t#)
               v#))))

;; ## Resolution
;;
;; It's more of a translation to the canonical value format, though.

(defn- resolve-scalar
  [opts {:keys [type-name non-null?] :as type} value]
  (assert-type (not (or (map? value) (sequential? value))) type value)
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
              (resolve-variable-value opts type-description field-value)])
           (into {})))))

(defn- resolve-named-type
  [{:keys [schema] :as opts}
   {:keys [type-name] :as type}
   value]
  (let [kind (get-in schema [:type->kind type-name])]
    (case kind
      (:scalar :enum) (resolve-scalar opts type value)
      :input-type     (resolve-input-type opts type value))))

(defn- resolve-list-type
  [opts {:keys [non-null? type-description]} value]
  (assert-type (sequential? value) type-description value)
  (some->> value
           (mapv #(resolve-variable-value opts type-description %))))

(defn- resolve-variable-value
  [opts {:keys [type-name non-null?] :as type} value]
  (if type-name
    (resolve-named-type opts type value)
    (resolve-list-type opts type value)))

(defn- resolve-variable
  [{:keys [variables] :as opts}
   {:keys [alumbra/variable-name
           alumbra/default-value
           alumbra/type]}]
  (let [v (get variables variable-name ::none)
        type (as-type-description type)]
    (if (= v ::none)
      (if default-value
        (resolve-value opts type default-value)
        ::none)
      (resolve-variable-value opts type v))))

(defn resolve-variables
  [opts {:keys [alumbra/variables]}]
  (->> (for [{:keys [alumbra/variable-name] :as variable} variables
             :let [variable-value (resolve-variable opts variable)]
             :when (not= variable-value ::none)]
         [variable-name variable-value])
       (into {})
       (assoc opts :variables)))
