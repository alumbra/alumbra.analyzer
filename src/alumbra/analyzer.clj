(ns alumbra.analyzer
  (:require [alumbra.analyzer
             [directives :as directives]
             [enums :as enums]
             [implementations :as implementations]
             [kinds :as kinds]
             [scalars :as scalars]
             [schema-root :as schema-root]
             [types :as types]
             [unions :as unions]
             [valid-fragment-spreads :as valid-fragment-spreads]]
            [alumbra.canonical
             [variables :refer [resolve-variables]]
             [fragments :refer [resolve-fragments]]
             [operations :refer [select-operation resolve-operation]]]
            [clojure.java.io :as io]))

;; ## Base Functionality

(defn- analyze-schema-ast
  "Analyze a GraphQL schema conforming to `:alumbra/schema` to produce a
   more compact representation conforming to `:alumbra/analyzed-schema`."
  [base-schema schema]
  (if-not (:alumbra/parser-errors schema)
    (-> base-schema
        (directives/analyze schema)
        (enums/analyze schema)
        (scalars/analyze schema)
        (schema-root/analyze schema)
        (types/analyze schema)
        (unions/analyze schema)
        (kinds/aggregate)
        (implementations/aggregate)
        (valid-fragment-spreads/aggregate))
    schema))

;; ## Protocol

(defprotocol Schema
  "Protocol for representations of GraphQL schemas."
  (analyze-schema* [schema base-schema parser-fn]
    "Convert the given GraphQL schema representation to a value conforming
     to `:alumbra/analyzed-schema`.

     Use [[analyze-schema]] to produce a self-contained schema, including base
     types, directives and introspection capabilities."))

(defn analyze-schemas*
  [schemas base-schema parser-fn]
  (reduce
    (fn [base-schema schema]
      (let [schema' (analyze-schema* schema base-schema parser-fn)]
        (if-not (:alumbra/parser-errors schema')
          schema'
          (reduced schema'))))
    base-schema schemas))

(extend-protocol Schema
  String
  (analyze-schema* [s base-schema parser-fn]
    (analyze-schema-ast
      base-schema
      (parser-fn s)))

  java.net.URL
  (analyze-schema* [url base-schema parser-fn]
    (analyze-schema* (slurp url) base-schema parser-fn))

  java.net.URI
  (analyze-schema* [uri base-schema parser-fn]
    (analyze-schema* (slurp uri) base-schema parser-fn))

  java.io.File
  (analyze-schema* [f base-schema parser-fn]
    (analyze-schema* (slurp f) base-schema parser-fn))

  java.io.InputStream
  (analyze-schema* [in base-schema parser-fn]
    (analyze-schema* (slurp in) base-schema parser-fn))

  clojure.lang.Sequential
  (analyze-schema* [sq base-schema parser-fn]
    (analyze-schemas* sq base-schema parser-fn)))

;; ## Introspection

(defn- add-introspection-query-fields
  [{:keys [schema-root] :as base-schema} parser-fn]
  (if-let [query-root-type (get-in schema-root [:schema-root-types "query"])]
    (analyze-schema*
      (format
        "extend type %s { __schema: __Schema!, __type(name: String!): __Type }"
        query-root-type)
      base-schema
      parser-fn)
    base-schema))

;; ## Public API

(def ^:private empty-schema
  {:types       {}
   :interfaces  {}
   :input-types {}
   :unions      {}})

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
         result (analyze-schema*
                  [base-schema intro-schema schema]
                  empty-schema
                  parser-fn)]
     (if-not (:alumbra/parser-errors result)
       (add-introspection-query-fields result parser-fn)
       result))))

(defn canonicalize-operation
  "Canonicalize a validated GraphQL document based on the given schema."
  ([analyzed-schema document]
   (canonicalize-operation analyzed-schema document nil {}))
  ([analyzed-schema document operation-name]
   (canonicalize-operation analyzed-schema document operation-name {}))
  ([analyzed-schema document operation-name variables]
   (let [{:keys [alumbra/fragments alumbra/operations]} document
         operation (select-operation operations operation-name)]
     (-> {:schema    analyzed-schema
          :variables variables}
         (resolve-variables operation)
         (resolve-fragments fragments)
         (resolve-operation operation)))))

(defn canonicalizer
  "Create a function canonicalizing GraphQL documents conforming to
   `:alumbra/document` based on the given schema. "
  [analyzed-schema]
  (partial canonicalize-operation analyzed-schema))
