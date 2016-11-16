(ns alumbra.analyzer
  (:require [alumbra.analyzer
             [directives :as directives]
             [enums :as enums]
             [kinds :as kinds]
             [scalars :as scalars]
             [schema-root :as schema-root]
             [types :as types]
             [unions :as unions]
             [valid-fragment-spreads :as valid-fragment-spreads]]
            [alumbra.canonical
             [fragments :refer [resolve-fragments]]
             [operations :refer [resolve-operation]]]))

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

(defn canonicalize-operation
  "Canonicalize a validated GraphQL document based on the given analyzed
   schema."
  ([analyzed-schema document]
   (canonicalize-operation analyzed-schema document nil {}))
  ([analyzed-schema document operation-name]
   (canonicalize-operation analyzed-schema document operation-name {}))
  ([analyzed-schema document operation-name variables]
   (let [{:keys [alumbra/fragments alumbra/operations]} document]
     (-> {:schema    analyzed-schema
          :variables variables}
         (resolve-fragments fragments)
         (resolve-operation operations operation-name)))))
