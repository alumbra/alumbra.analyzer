(ns alumbra.analyzer.inline-directives
  (:require [alumbra.analyzer.value :refer [read-value]]))

(defn read-inline-directives
  [{:keys [alumbra/directives]}]
  (->> (for [{:keys [alumbra/directive-name alumbra/arguments]} directives]
         [directive-name
          (->> (for [{:keys [alumbra/argument-name
                             alumbra/argument-value]} arguments]
                 [argument-name (read-value argument-value)])
               (into {}))])
       (into {})))
