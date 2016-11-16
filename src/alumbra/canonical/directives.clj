(ns alumbra.canonical.directives
  (:require [alumbra.canonical.arguments :refer [resolve-arguments]]))

(defn resolve-directives
  [opts directives]
  (for [{:keys [alumbra/directive-name
                alumbra/arguments]} directives]
    {:directive-name directive-name
     :arguments (resolve-arguments opts arguments)}))
