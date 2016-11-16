(ns alumbra.analyzer
  (:require [alumbra.analyzer
             [directives :as directives]
             [enums :as enums]
             [kinds :as kinds]
             [scalars :as scalars]
             [schema-root :as schema-root]
             [types :as types]
             [unions :as unions]
             [valid-fragment-spreads :as valid-fragment-spreads]]))

(defn analyze-schema
  "Analyze a GraphQL schema conforming to `:alumbra/schema` to produce a
   more compact representation conforming to `:alumbra/analyzed-schema`."
  [schema]
  {:pre [(not (:alumbra/parser-errors schema))]}
  (-> (merge
        (directives/analyze schema)
        (enums/analyze schema)
        (scalars/analyze schema)
        (schema-root/analyze schema)
        (types/analyze schema)
        (unions/analyze schema))
      (kinds/aggregate)
      (valid-fragment-spreads/aggregate)))
