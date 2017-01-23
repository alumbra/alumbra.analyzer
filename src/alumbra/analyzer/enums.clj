(ns alumbra.analyzer.enums
  (:require [alumbra.analyzer.inline-directives
             :refer [read-inline-directives]]))

(defn analyze
  "Analyze enum definitions in a GraphQL schema conforming to
   `:alumbra/schema`."
  [base-schema {:keys [alumbra/enum-definitions]}]
  (->> (reduce
         (fn [result {:keys [alumbra/type-name
                             alumbra/enum-fields] :as x}]
           (let [enum-values (into #{} (map :alumbra/enum) enum-fields)]
             (assoc result type-name
                    {:type-name type-name
                     :enum-values       enum-values
                     :inline-directives (read-inline-directives x)})))
         {} enum-definitions)
       (update base-schema :enums (fnil into {}))))
