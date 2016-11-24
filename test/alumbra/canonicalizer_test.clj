(ns alumbra.canonicalizer-test
  (:require [clojure.test.check
             [clojure-test :refer [defspec]]
             [generators :as gen]
             [properties :as prop]]
            [clojure.test :refer :all]
            [alumbra.parser :as ql]
            [alumbra.analyzer :as analyzer]
            [alumbra.generators
             [common :refer [rarely -name]]
             [directives :refer [-directives]]]
            [alumbra.spec]
            [clojure.string :as string]
            [clojure.spec :as s]))

;; ## Schema

(def schema
  (analyzer/analyze-schema
    "type Person { id:ID!, name:String!, pet: Pet }
     interface Pet { id:ID!, name:String! }
     type Cat implements Pet { id:ID!, name:String!, meows: Boolean }
     type Dog implements Pet { id:ID!, name:String!, barks: Boolean }
     union CatOrDog = Cat | Dog
     type QueryRoot { pet(name: String!): Pet, me: Person!, randomCat: Cat }
     schema { query: QueryRoot }"
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
        #(analyzer/canonicalize-operation schema %)
        ql/parse-document))

;; ## Generators

(defn gen-directives
  []
  (gen/fmap
    #(some-> % (string/replace "$" ""))
    (rarely -directives)))

(defn gen-selection-set
  [fields->gen]
  (gen/let [fields (gen/set (gen/elements (keys fields->gen))
                            {:min-elements 1})]
    (let [fields (seq fields)
          gens   (map fields->gen fields)]
      (->> gens
           (map (fn [f] (if f (f) (gen/return nil))))
           (apply gen/tuple)
           (gen/fmap
             (fn [subselections]
               (->> (map
                      (fn [field subselection]
                        (if subselection
                          (str field " " subselection)
                          field))
                      fields subselections)
                    (string/join ",")
                    (format "{%s}"))))))))

(defn gen-cat-selection-set
  []
  (gen-selection-set
    {"id"    gen-directives,
     "name"  gen-directives,
     "meows" gen-directives}))

(defn gen-dog-selection-set
  []
  (gen-selection-set
    {"id"    gen-directives,
     "name"  gen-directives,
     "barks" gen-directives}))

(defn gen-pet-selection-set
  []
  (gen-selection-set
    {"id"         gen-directives,
     "name"       gen-directives,
     "... on Cat" gen-cat-selection-set
     "... on Dog" gen-dog-selection-set}))

(defn gen-person-selection-set
  []
  (gen-selection-set
    {"id"   gen-directives
     "name" gen-directives
     "pet"  gen-pet-selection-set}))

(defn gen-type-selection-set
  []
  (gen-selection-set
    {"kind"        nil
     "name"        nil
     "description" nil
     "possibleTypes" #(gen/one-of
                        [(gen-type-selection-set)
                         (gen/return "{name}")])}))

(defn gen-schema-selection-set
  []
  (gen-selection-set
    {"types" gen-type-selection-set}))

(defn gen-query-root-selection-set
  []
  (gen-selection-set
    {"pet"      (let [g (gen-pet-selection-set)]
                  (constantly
                    (gen/let [n -name
                              d (gen-directives)
                              s g]
                      (str "(name: \"" n "\") "
                           (some-> d (str " "))
                           s))))
     "me"       gen-person-selection-set
     "__schema" gen-schema-selection-set}))

;; ## Tests

(defspec t-canonicalizer-conforms-to-spec 500
  (prop/for-all
    [document (gen-query-root-selection-set)]
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
