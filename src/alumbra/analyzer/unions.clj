(ns alumbra.analyzer.unions
  (:require [alumbra.analyzer.types.default-fields
             :refer [default-type-fields]]
            [alumbra.analyzer.inline-directives
             :refer [read-inline-directives]]
            [com.rpl.specter :refer [traverse ALL collect-one]]))

(defn analyze
  "Analyze union definitions in a GraphQL schema conforming to
   `:alumbra/schema`."
  [base-schema {:keys [alumbra/union-definitions]}]
  (->> union-definitions
       (traverse
         [ALL
          (collect-one)
          :alumbra/union-types
          ALL
          :alumbra/type-name])
       (reduce
         (fn [result [{:keys [alumbra/type-name] :as x} union-type-name]]
           (update result
                   type-name
                   (fnil
                     #(update % :union-types conj union-type-name)
                     {:type-name   type-name
                      :fields      (default-type-fields type-name)
                      :inline-directives (read-inline-directives x)
                      :union-types #{}})))
         {})
       (update base-schema :unions (fnil into {}))))
