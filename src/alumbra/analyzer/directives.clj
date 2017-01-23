(ns alumbra.analyzer.directives
  (:require [alumbra.analyzer.types.arguments
             :refer [read-arguments]]))

(defn analyze
  "Analyze directive definitions in a GraphQL schema conforming to
   `:alumbra/schema`."
  [base-schema {:keys [alumbra/directive-definitions]}]
  (->> directive-definitions
       (reduce
         (fn [result {:keys [alumbra/directive-name
                             alumbra/directive-locations
                             alumbra/argument-definitions] :as x}]
           (assoc result
                  directive-name
                  {:directive-locations (set directive-locations)
                   :arguments (read-arguments argument-definitions)}))
         {})
       (update base-schema :directives (fnil into {}))))
