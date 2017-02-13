(ns alumbra.canonical.arguments
  (:require [alumbra.canonical.value :refer [resolve-value]]))

(defn resolve-arguments
  [opts argument-types arguments]
  (->> (for [{:keys [alumbra/argument-name
                     alumbra/argument-value]} arguments
             :let [argument-type (get argument-types argument-name)]]
         [argument-name (resolve-value opts argument-type argument-value)])
       (into {})))
