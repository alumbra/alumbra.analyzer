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
             [operations :refer [resolve-operation]]]
            [clojure.java.io :as io]))

;; ## Base Functionality

(defn- analyze-schema-ast
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

(defn- merge-schemas
  "Merge a series of analyzed (!) GraphQL schemas."
  [analyzed-schemas]
  {:pre [(not-any? :alumbra/parser-errors analyzed-schemas)]}
  (apply merge-with into analyzed-schemas))

;; ## Protocol

(defprotocol IReadable
  "Protocol for values that can be converted to strings."
  (as-strings [schema]
    "Convert the given GraphQL schema/document to its string representation."))

(extend-protocol IReadable
  String
  (as-strings [s]
    [s])

  java.net.URL
  (as-strings [uri]
    [(slurp uri)])

  java.net.URI
  (as-strings [uri]
    [(slurp uri)])

  java.io.File
  (as-strings [f]
    [(slurp f)])

  java.io.InputStream
  (as-strings [in]
    [(slurp in)])

  clojure.lang.Sequential
  (as-strings [sq]
    (mapcat as-strings sq)))

;; ## Introspection

(defn- introspection-query-fields
  [parser-fn]
  (-> "type __Introspection {
         __schema: __Schema!
         __type(name: String!): __Type
       }"
      (parser-fn)
      (analyze-schema-ast)
      (get-in [:types "__Introspection" :fields])
      (dissoc "__typename")))

(defn- introspection-query-fields-for
  [parser-fn type-name]
  (-> (introspection-query-fields parser-fn)
      (assoc-in ["__schema" :containing-type-name] type-name)
      (assoc-in ["__type" :containing-type-name] type-name)))

(defn- add-introspection-queries
  [parser-fn {:keys [schema-root types] :as schema}]
  (if-let [root-type (get-in schema-root [:schema-root-types "query"])]
    (if (contains? types root-type)
      (->> (introspection-query-fields-for parser-fn root-type)
           (update-in schema [:types root-type :fields] merge))
      schema)
    schema))

;; ## Public API

(defn analyze-schema
  "Given a schema (implementing `IReadable`), as well as a
   parser function, generate a value conforming to `:alumbra/analyzed-schema`.

   This will already include the GraphQL introspection schema."
  [parser-fn schema
   & [{:keys [introspection-schema]
       :or {introspection-schema
            (io/resource "alumbra/GraphQLIntrospection.graphql")}}]]
  (let [asts (map parser-fn (as-strings [introspection-schema schema]))
        errors (mapcat :alumbra/parser-errors asts)]
    (if (seq errors)
      {:alumbra/parser-errors errors}
      (->> (map analyze-schema-ast asts)
           (merge-schemas)
           (add-introspection-queries parser-fn)))))

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
