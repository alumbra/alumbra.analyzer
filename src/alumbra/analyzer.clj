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

;; ## Base Functionality

(defn analyze-schema-ast
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

(defn merge-schemas
  "Merge a series of analyzed (!) GraphQL schemas."
  [analyzed-schemas]
  {:pre [(not-any? :alumbra/parser-errors analyzed-schemas)]}
  (apply merge-with into analyzed-schemas))

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

;; ## Protocol

(defprotocol IReadable
  "Protocol for values that can be converted to strings."
  (as-string [schema]
    "Convert the given GraphQL schema/document to its string representation."))

(extend-protocol IReadable
  String
  (as-string [s]
    s)

  java.net.URL
  (as-string [uri]
    (slurp uri))

  java.net.URI
  (as-string [uri]
    (slurp uri))

  java.io.File
  (as-string [f]
    (slurp f))

  java.io.InputStream
  (as-string [in]
    (slurp in)))

(defn analyze-schemas
  "Given a series of schemas (implementing `IReadable`), as well as a
   parser function, generate a combined analyzed schema."
  [parser-fn schemas]
  (->> (map as-string schemas)
       (map parser-fn)
       (map analyze-schema-ast)
       (merge-schemas)))
