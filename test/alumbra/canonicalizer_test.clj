(ns alumbra.canonicalizer-test
  (:require [clojure.test.check
             [clojure-test :refer [defspec]]
             [generators :as gen]
             [properties :as prop]]
            [clojure.test :refer :all]
            [alumbra.parser :as ql]
            [alumbra.analyzer :as analyzer]
            [alumbra.generators :as alumbra-gen]
            [alumbra.generators.raw.common :refer [-name]]
            [alumbra.spec]
            [clojure.string :as string]
            [clojure.spec :as s]))

;; ## Schema

(def schema
  (analyzer/analyze-schema
    "enum Emotion { HAPPY HAPPIER }
     input CatQuery { emotions: [Emotion!]! }
     type Person { id:ID!, name:String!, pet: Pet }
     interface Pet { id:ID!, name:String! }
     type Cat implements Pet { id:ID!, name:String!, meows: Boolean }
     type Dog implements Pet { id:ID!, name:String!, barks: Boolean }
     union CatOrDog = Cat | Dog
     type QueryRoot { pet(name: String!): Pet, me: Person!, randomCat(q: CatQuery): Cat }
     type MutationRoot { addPet(personId: ID!, name: String): Person }
     schema { query: QueryRoot, mutation: MutationRoot }"
    ql/parse-schema))

;; ## Helper

(defn shape-of
  [{:keys [field-alias selection-set type-condition]}]
  (if selection-set
    (cond->> (mapcat shape-of selection-set)
      type-condition (cons type-condition)
      field-alias (list field-alias))
    [field-alias]))

(def canonicalize-shape
  (comp shape-of
        (analyzer/canonicalizer schema)
        ql/parse-document))

(def gen-operation
  (alumbra-gen/operation schema))

;; ## Tests

(defspec t-canonicalizer-conforms-to-spec 1000
  (prop/for-all
    [document (gen/one-of
                [(gen-operation :query)
                 (gen-operation :mutation)])]
    (let [ast (ql/parse-document document)]
      (s/valid? :alumbra/canonical-operation
                (analyzer/canonicalize-operation schema ast)))))

(deftest t-canonicalizer-fragment-inlining
  (are [expected-shape query] (= expected-shape (canonicalize-shape query))
       ;; non-inlineable fragments
       '("pet" (#{"Cat"} "name"))
       "{ pet (name:\"x\") { ... on Cat { name } } }"

       '("pet" (#{"Cat"} "name"))
       "{ pet (name:\"x\") { ... X } }
        fragment X on Cat { name }"

       ;; exact fragment type match
       '("randomCat" ("name"))
       "{ randomCat { ... on Cat { name } } }"

       '("randomCat" ("name"))
       "{ randomCat { ... X } }
        fragment X on Cat { name }"

       ;; interface fragment type match
       '("randomCat" ("name"))
       "{ randomCat { ... on Pet { name } } }"

       '("randomCat" ("name"))
       "{ randomCat { ... X } }
        fragment X on Pet { name }"

       ;; union fragment type match
       '("randomCat" ("__typename"))
       "{ randomCat { ... on CatOrDog { __typename } } }"

       '("randomCat" ("__typename"))
       "{ randomCat { ... X } }
        fragment X on CatOrDog { __typename }"

       ;; nested inlineable fragments
       '("randomCat" ("name"))
       "{ randomCat { ... on Cat { ... on Pet { name } } } }"

       '("randomCat" ("name"))
       "{ randomCat { ... on Cat { ... X } } }
        fragment X on Pet { name }"))

(deftest t-variables
  (letfn [(canonicalize [query & [variables]]
            (let [ast (ql/parse-document query)]
              (if variables
                (analyzer/canonicalize-operation schema ast nil variables)
                (analyzer/canonicalize-operation schema ast))))]
    (testing "default value."
      (is (= (canonicalize
               "{ randomCat(q: {emotions: [HAPPY HAPPIER]}) { name } }")
             (canonicalize
               "query ($q: CatQuery!) { randomCat(q: $q) { name } }"
               {"q" {"emotions" ["HAPPY" "HAPPIER"]}})
             (canonicalize
               "query ($q: CatQuery = {emotions: [HAPPY HAPPIER]}) {
                randomCat(q: $q) { name }
                }"))))
    (testing "explicit null value."
      (is (= (canonicalize
               "{ randomCat(q: null) { name } }")
             (canonicalize
               "query ($q: CatQuery) { randomCat(q: $q) { name } }"
               {"q" nil})
             (canonicalize
               "query ($q: CatQuery = {emotions: [HAPPY HAPPIER]}) {
                randomCat(q: $q) { name }
                }"
               {"q" nil}))))
    (testing "nullable variable."
      (let [query "query ($q: CatQuery) { randomCat(q: $q) { name } }"]
        (is (= (canonicalize query {})
               (canonicalize query {"q" nil})))))
    (testing "non-nullable variable."
      (let [query "query ($q: CatQuery!) { randomCat(q: $q) { name } }"]
        (is (thrown-with-msg?
              IllegalArgumentException
              #"null value not allowed"
              (canonicalize query {})))
        (is (thrown-with-msg?
              IllegalArgumentException
              #"null value not allowed"
              (canonicalize query {"q" nil})))))))

(deftest t-variable-type-mismatch
  (letfn [(canonicalize [query variables]
            (analyzer/canonicalize-operation
              schema
              (ql/parse-document query)
              nil
              variables))]
    (testing "input type mismatch."
      (is (thrown-with-msg?
            IllegalArgumentException
            #"does not match expected type 'CatQuery!'"
            (canonicalize
              "query ($q: CatQuery!) {
               randomCat(q: $q) { name }
               }"
              {"q" true}))))
    (testing "list type mismatch."
      (is (thrown-with-msg?
            IllegalArgumentException
            #"does not match expected type '\[Emotion!\]'"
            (canonicalize
              "query ($emotions: [Emotion!]) {
               randomCat(q: {emotions: $emotions}) { name }
               }"
              {"emotions" "HAPPY"}))))
    (testing "enum type mismatch."
      (is (thrown-with-msg?
            IllegalArgumentException
            #"does not match expected type 'Emotion!'"
            (canonicalize
              "query ($emotion: Emotion!) {
               randomCat(q: {emotions: [$emotion]}) { name }
               }"
              {"emotion" "UNKNOWN"}))))
    (testing "nullability mismatch."
      (is (thrown-with-msg?
            IllegalArgumentException
            #"null value not allowed for expected type 'CatQuery!'"
            (canonicalize
              "query ($q: CatQuery!) {
               randomCat(q: $q) { name }
               }"
              {"q" nil}))))))

(deftest t-fragment-variables
  (letfn [(canonicalize [query & [variables]]
            (let [ast (ql/parse-document query)]
              (if variables
                (analyzer/canonicalize-operation schema ast nil variables)
                (analyzer/canonicalize-operation schema ast))))]
    (is (= {"q"
            {"emotions"
             [{:type-name "Emotion", :non-null? true, :value "HAPPY"}]}}
           (-> (canonicalize
                 "query ($q: CatQuery = {emotions: [HAPPY HAPPIER]}) {
                  randomCat(q: $q) @test { name }
                  }"
                 {"q" {"emotions" ["HAPPY"]}})
               :selection-set
               first
               :arguments)
           (-> (canonicalize
                 "query ($q: CatQuery = {emotions: [HAPPY HAPPIER]}) {
                    ...F0
                  }
                  fragment F0 on QueryRoot @test {
                    randomCat(q: $q) { name }
                  }"
                 {"q" {"emotions" ["HAPPY"]}})
               :selection-set
               first
               :selection-set
               first
               :arguments)))))
