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
