(ns alumbra.analyzer.implementations)

(defn aggregate
  [{:keys [interfaces types] :as schema}]
  (reduce
    (fn [schema {:keys [type-name implements]}]
      (reduce
        (fn [schema interface-name]
          (cond-> schema
            (contains? interfaces interface-name)
            (update-in [:interfaces interface-name :implemented-by]
                       conj
                       type-name)))
        schema implements))
    schema (vals types)))
