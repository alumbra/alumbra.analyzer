(ns alumbra.analyzer.schema-root
  (:require [alumbra.analyzer.inline-directives
             :refer [read-inline-directives]]
            [com.rpl.specter :refer [traverse ALL collect-one]]))

(defn analyze
  [{:keys [alumbra/schema-definitions]}]
  (let [{:keys [alumbra/schema-fields] :as x} (first schema-definitions)]
    {:schema-root
     {:schema-root-types
      (->> schema-fields
           (traverse
             [ALL
              (collect-one :alumbra/operation-type)
              :alumbra/schema-type
              :alumbra/type-name])
           (into {}))
      :inline-directives
      (read-inline-directives x)}}))
