(ns alumbra.analyzer.types.type-description)

(defn- read-type-name
  [{:keys [alumbra/type-class
           alumbra/type-name
           alumbra/element-type]}]
  (case type-class
    :named-type type-name
    :list-type  (read-type-name element-type)))

(defn- read-type-description
  [{:keys [alumbra/type-class
           alumbra/non-null?
           alumbra/type-name
           alumbra/element-type]}]
  (case type-class
    :named-type {:non-null? non-null?
                 :type-name type-name}
    :list-type {:non-null? non-null?
                :type-description
                (read-type-description element-type)}))

(defn- read-non-null?
  [{:keys [alumbra/non-null?]}]
  non-null?)

(defn describe-type
  [type]
  {:type-description (read-type-description type)
   :type-name        (read-type-name type)
   :non-null?        (read-non-null? type)})
