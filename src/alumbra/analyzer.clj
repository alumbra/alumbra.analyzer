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
  (if-not (:alumbra/parser-errors schema)
    (-> (merge
          (directives/analyze schema)
          (enums/analyze schema)
          (scalars/analyze schema)
          (schema-root/analyze schema)
          (types/analyze schema)
          (unions/analyze schema))
        (kinds/aggregate)
        (valid-fragment-spreads/aggregate))
    schema))

(defn- merge-schemas
  "Merge a series of analyzed (!) GraphQL schemas."
  [analyzed-schemas]
  (let [errors (mapcat :alumbra/parser-errors analyzed-schemas)]
    (if (seq errors)
      {:alumbra/parser-errors errors}
      (apply merge-with into analyzed-schemas))))

;; ## Protocol

(defprotocol Schema
  "Protocol for representations of GraphQL schemas."
  (analyze-schema* [schema parser-fn]
    "Convert the given GraphQL schema representation to a value conforming
     to `:alumbra/analyzed-schema`.

     Use [[analyze-schema]] to produce a self-contained schema, including base
     types, directives and introspection capabilities."))

(extend-protocol Schema
  String
  (analyze-schema* [s parser-fn]
    (analyze-schema-ast
      (parser-fn s)))

  clojure.lang.IPersistentMap
  (analyze-schema* [m _]
    (if (contains? m :type->kind)
      m
      (analyze-schema-ast m)))

  java.net.URL
  (analyze-schema* [url parser-fn]
    (analyze-schema* (slurp url) parser-fn))

  java.net.URI
  (analyze-schema* [uri parser-fn]
    (analyze-schema* (slurp uri) parser-fn))

  java.io.File
  (analyze-schema* [f parser-fn]
    (analyze-schema* (slurp f) parser-fn))

  java.io.InputStream
  (analyze-schema* [in parser-fn]
    (analyze-schema* (slurp in) parser-fn))

  clojure.lang.Sequential
  (analyze-schema* [sq parser-fn]
    (merge-schemas
      (keep
        #(when %
           (analyze-schema* % parser-fn))
        sq))))

;; ## Introspection

(defn- introspection-query-fields
  [parser-fn]
  (-> "type __Introspection {
         __schema: __Schema!
         __type(name: String!): __Type
       }"
      (analyze-schema* parser-fn)
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
  "Given a schema (implementing `IReadable`), as well as a parser function,
   generate a value conforming to `:alumbra/analyzed-schema`.

   ```clojure
   (analyze-schema
     \"type Person { name: String! }
      schema { query: Person }\"
     alumbra.parser/parse-schema)
   ```

   By default, base and introspection schemas bundled with this library will
   be used. You can exclude them by explicitly supplying `nil` and replace
   them by passing a `Schema` value."
  ([schema parser-fn]
   (analyze-schema schema parser-fn {}))
  ([schema parser-fn {:keys [base-schema introspection-schema]}]
   (let [base-schema  (or base-schema
                          (io/resource "alumbra/GraphQLBase.graphql"))
         intro-schema (or introspection-schema
                          (io/resource "alumbra/GraphQLIntrospection.graphql"))
         result (analyze-schema* [base-schema intro-schema schema] parser-fn)]
     (if-not (:alumbra/parser-errors result)
       (add-introspection-queries parser-fn result)
       result))))

(defn canonicalize-operation
  "Canonicalize a validated GraphQL document based on the given schema."
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

(defn canonicalizer
  "Create a function canonicalizing GraphQL documents conforming to
   `:alumbra/document` based on the given schema. "
  [analyzed-schema]
  (partial canonicalize-operation analyzed-schema))
