(ns alumbra.canonical.variables
  (:require [alumbra.canonical
             [types :refer [as-type-description]]
             [value :refer [resolve-value]]
             [literal-value :refer [resolve-literal-value]]]))

(defn- resolve-variable
  [{:keys [variables] :as opts}
   {:keys [alumbra/variable-name
           alumbra/default-value
           alumbra/type]}]
  (try
    (let [v (get variables variable-name ::none)
          type (as-type-description type)]
      (if (= v ::none)
        (if default-value
          (resolve-value opts type default-value)
          (resolve-literal-value opts type nil))
        (resolve-literal-value opts type v)))
    (catch IllegalArgumentException t
      (throw
        (IllegalArgumentException.
          (format "Variable '$%s': %s"
                  variable-name
                  (.getMessage t))
          t)))))

(defn resolve-variables
  [opts {:keys [alumbra/variables]}]
  (->> (for [{:keys [alumbra/variable-name] :as variable} variables
             :let [variable-value (resolve-variable opts variable)]]
         [variable-name variable-value])
       (into {})
       (assoc opts :variables)))
