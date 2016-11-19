(ns alumbra.analyzer.kinds)

(defn- aggregate-map
  [schema k kind]
  (->> (get schema k)
       (keys)
       (map #(vector % kind))
       (update schema :type->kind into)))

(defn aggregate
  [schema]
  (-> schema
      (assoc :type->kind {})
      (aggregate-map :types       :type)
      (aggregate-map :interfaces  :interface)
      (aggregate-map :unions      :union)
      (aggregate-map :directives  :directive)
      (aggregate-map :input-types :input-type)
      (aggregate-map :enums       :enum)
      (aggregate-map :scalars     :scalar)))
