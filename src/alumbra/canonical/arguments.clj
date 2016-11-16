(ns alumbra.canonical.arguments
  (:require [alumbra.canonical.value :refer [resolve-value]]))

(defn resolve-arguments
  [opts arguments]
  (->> (for [{:keys [alumbra/argument-name
                     alumbra/argument-value]} arguments]
         [argument-name (resolve-value opts argument-value)])
       (into {})))
