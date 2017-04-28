(ns alumbra.analyzer-test
  (:require [clojure.test.check
             [clojure-test :refer [defspec]]
             [properties :as prop]]
            [clojure.test :refer :all]
            [alumbra.parser :as ql]
            [alumbra.analyzer :as analyzer]
            [alumbra.spec]
            [alumbra.generators :as alumbra-gen]
            [clojure.spec :as s]))

(defn- analyze
  [schema]
  (analyzer/analyze-schema schema ql/parse-schema))

(defspec t-analyzer-conforms-to-spec 500
  (prop/for-all
    [schema (alumbra-gen/raw-schema)]
    (let [analyzed-schema (analyze schema)]
      (or (:alumbra/parser-errors analyzed-schema)
          (s/valid? :alumbra/analyzed-schema analyzed-schema)))))

(deftest t-analyzer-merges-extend-type
  (let [schema (analyze
                 ["type QueryRoot { message: String! }
                   schema { query: QueryRoot }"
                  "extend type QueryRoot { messageCount: Int! }"])]
    (is (not (:alumbra/parser-errors schema)))
    (is (= #{"message" "messageCount" "__typename" "__schema" "__type"}
           (-> (get-in schema [:types "QueryRoot" :fields]) keys set)))
    (is (= "QueryRoot"
           (get-in schema [:schema-root :schema-root-types "query"])))))

(deftest t-analyzer-interface-implementations
  (let [declaration          "interface SomeInterface { x: Int }"
        implementation       "type SomeType implements SomeInterface { x: Int }"
        declaration-first    (analyze [declaration implementation])
        implementation-first (analyze [implementation declaration])
        implemented-by       #(get-in % [:interfaces "SomeInterface" :implemented-by])
        implements           #(get-in % [:types "SomeType" :implements])]
    (is (= (implemented-by declaration-first)
           (implemented-by implementation-first)
           #{"SomeType"}))
    (is (= (implements declaration-first)
           (implements implementation-first)
           #{"SomeInterface"}))))

(deftest t-analyzer-valid-fragment-spreads
  (let [schema (analyze
                 "interface InterfaceA { id: ID! }
                  interface InterfaceB { name: String! }
                  interface InterfaceC { age: Int! }
                  type Person implements InterfaceA, InterfaceB { id: ID!, name: String! }
                  type QueryRoot { value: InterfaceA }
                  schema { query: QueryRoot }")
        for-interface #(get-in schema [:interfaces % :valid-fragment-spreads])
        for-type #(get-in schema [:types % :valid-fragment-spreads])]
    (is (not (:alumbra/parser-errors schema)))
    (is (= #{"InterfaceC"}
           (for-interface "InterfaceC")))
    (is (= #{"InterfaceA" "InterfaceB" "Person"}
           (for-interface "InterfaceA")
           (for-interface "InterfaceB")))
    (is (= #{"Person" "InterfaceA" "InterfaceB"}
           (for-type "Person")))))
