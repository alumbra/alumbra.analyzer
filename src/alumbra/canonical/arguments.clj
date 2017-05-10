(ns alumbra.canonical.arguments
  (:require [alumbra.canonical
             [literal-value :refer [resolve-literal-value]]
             [value :refer [resolve-value]]]))

(defn- add-default-values
  [opts argument-types result]
  (let [missing-arguments (apply dissoc argument-types (keys result))]
    (reduce
      (fn [result [argument-name {:keys [default-value] :as type}]]
        (if (and default-value
                 (not (contains? result argument-name)))
          (assoc result
                 argument-name
                 (resolve-literal-value opts type default-value))
          result))
      result missing-arguments)))

(defn resolve-arguments
  [opts argument-types arguments]
  (->> (for [{:keys [alumbra/argument-name
                     alumbra/argument-value]} arguments
             :let [{:keys [type-description]} (get argument-types argument-name)]]
         [argument-name (resolve-value opts type-description argument-value)])
       (into {})
       (add-default-values opts argument-types)))
