(ns alumbra.analyzer.valid-fragment-spreads
  (:require [clojure.set :as set]))

(defn- add-valid-fragment-spreads-to-union
  [{:keys [types]} union-types allowed-types]
  (->> (mapcat
         (comp :valid-fragment-spreads types)
         union-types)
       (into allowed-types)))

(defn- add-matching-unions
  [{:keys [unions]} allowed-types]
  (->> unions
       (keep
         (fn [[type-name {:keys [union-types]}]]
           (when (seq (set/intersection allowed-types union-types))
             type-name)))
       (into allowed-types)))

(defn- add-valid-fragment-spreads-to-type
  [schema {:keys [type-name
                  implements
                  implemented-by
                  union-types] :as type}]
  (->> (concat implements implemented-by union-types)
       (into #{type-name})
       (add-matching-unions schema)
       (add-valid-fragment-spreads-to-union schema union-types)
       (assoc type :valid-fragment-spreads)))

(defn- add-valid-fragment-spreads
  [schema k]
  (->> (fn [m]
         (->> (for [[k v] m]
                [k (add-valid-fragment-spreads-to-type schema v)])
              (into {})))
       (update schema k)))

(defn aggregate
  [{:keys [unions] :as schema}]
  (-> schema
      (add-valid-fragment-spreads :types)
      (add-valid-fragment-spreads :interfaces)
      (add-valid-fragment-spreads :unions)))
