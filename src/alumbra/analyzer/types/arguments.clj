(ns alumbra.analyzer.types.arguments
  (:require [alumbra.analyzer.types.type-description
             :refer [describe-type]]
            [alumbra.analyzer.inline-directives
             :refer [read-inline-directives]]
            [alumbra.analyzer.value
             :refer [read-value]]))

(defn read-arguments
  [arguments]
  (reduce
    (fn [result {:keys [alumbra/argument-name
                        alumbra/argument-type
                        alumbra/default-value]
                 :as argument}]
      (->> {:argument-name     argument-name
            :inline-directives (read-inline-directives argument)}
           (merge
             (describe-type argument-type)
             (when default-value
               {:default-value (read-value default-value)}))
           (assoc result argument-name)))
    {} arguments))
