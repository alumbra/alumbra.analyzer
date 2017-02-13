(ns alumbra.canonical.directives
  (:require [alumbra.canonical.arguments :refer [resolve-arguments]]))

(defn resolve-directives
  [opts directives]
  (for [{:keys [alumbra/directive-name
                alumbra/arguments]} directives
        :let [argument-types
              (get-in opts [:schema :directives directive-name :arguments])]]
    {:directive-name directive-name
     :arguments (resolve-arguments opts argument-types arguments)}))
