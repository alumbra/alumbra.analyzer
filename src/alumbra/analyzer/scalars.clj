(ns alumbra.analyzer.scalars
  (:require [alumbra.analyzer.inline-directives
             :refer [read-inline-directives]]))

(defn analyze
  "Analyze scalar definitions in a GraphQL schema conforming to
   `:alumbra/schema`."
  [{:keys [alumbra/scalar-definitions]}]
  {:scalars
   (->> (for [{:keys [alumbra/type-name] :as x} scalar-definitions]
          [type-name
           {:type-name         type-name
            :inline-directives (read-inline-directives x)}])
        (into {}))})
