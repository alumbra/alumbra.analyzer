(ns alumbra.analyzer-test
  (:require [clojure.test.check
             [clojure-test :refer [defspec]]
             [properties :as prop]]
            [alumbra.parser :as ql]
            [alumbra.analyzer :as analyzer]
            [alumbra.spec]
            [alumbra.generators :as alumbra-gen]
            [clojure.spec :as s]))

(defspec t-analyzer-conforms-to-spec 500
  (prop/for-all
    [schema (alumbra-gen/raw-schema)]
    (let [analyzed-schema (analyzer/analyze-schema schema ql/parse-schema)]
      (or (:alumbra/parser-errors analyzed-schema)
          (s/valid? :alumbra/analyzed-schema analyzed-schema)))))
