(ns alumbra.analyzer.types.arguments
  (:require [alumbra.analyzer.types.type-description
             :refer [describe-type]]
            [alumbra.analyzer.inline-directives
             :refer [read-inline-directives]]))

(defn read-arguments
  [arguments]
  (reduce
    (fn [result {:keys [alumbra/argument-name
                        alumbra/argument-type] :as x}]
      (->> {:argument-name     argument-name
            :inline-directives (read-inline-directives x)}
           (merge (describe-type argument-type))
           (assoc result argument-name)))
    {} arguments))
