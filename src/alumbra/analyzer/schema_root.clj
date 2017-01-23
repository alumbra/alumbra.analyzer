(ns alumbra.analyzer.schema-root
  (:require [alumbra.analyzer.inline-directives
             :refer [read-inline-directives]]
            [com.rpl.specter :refer [traverse ALL collect-one]]))

(defn analyze
  [base-schema {:keys [alumbra/schema-definitions]}]
  (let [{:keys [alumbra/schema-fields] :as schema-def}
        (first schema-definitions)
        root-types (->> schema-fields
                        (traverse
                          [ALL
                           (collect-one :alumbra/operation-type)
                           :alumbra/schema-type
                           :alumbra/type-name])
                        (into {}))
        inline-directives (read-inline-directives schema-def)]
    (-> base-schema
        (update-in [:schema-root :schema-root-types] (fnil into {}) root-types)
        (update-in [:schema-root :inline-directives]
                   (fn [existing-directives]
                     (or existing-directives
                         inline-directives))))))
