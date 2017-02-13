(ns alumbra.canonical.types)

(defn as-type-description
  "Convert `:alumbra/type` to generic type description."
  [{:keys [alumbra/type-class] :as t}]
  (case type-class
    :named-type
    (let [{:keys [alumbra/type-name alumbra/non-null?]} t]
      {:type-name type-name
       :non-null? non-null?})

    :list-type
    (let [{:keys [alumbra/element-type alumbra/non-null?]} t]
      {:non-null?        non-null?
       :type-description (as-type-description element-type)})
    t))
