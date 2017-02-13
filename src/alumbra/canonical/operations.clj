(ns alumbra.canonical.operations
  (:require [alumbra.canonical
             [selection-set :refer [resolve-selection-set]]
             [directives :refer [resolve-directives]]
             [variables :refer [resolve-variables]]]))

;; ## Helpers

(defn- root-type
  [{{:keys [schema-root]} :schema}
   {:keys [alumbra/operation-type]}]
  (get-in schema-root [:schema-root-types operation-type]))

;; ## Operation Resolution

(defn- select-operation
  [operations operation-name']
  (cond operation-name'
        (or (some
              (fn [{:keys [alumbra/operation-name] :as operation}]
                (when (= operation-name operation-name')
                  operation))
              operations)
            (throw
              (IllegalArgumentException. "unknown operation")))

        (next operations)
        (throw
          (IllegalArgumentException. "no operation name supplied"))

        :else
        (first operations)))

(defn- resolve-operation*
  [opts {:keys [alumbra/selection-set
                alumbra/directives
                alumbra/operation-type
                alumbra/operation-name] :as op}]
  (let [opts (-> opts
                 (assoc :scope-type (root-type opts op))
                 (resolve-variables op))
        selection (resolve-selection-set opts selection-set)]
    {:operation-name operation-name
     :operation-type operation-type
     :selection-set  selection
     :directives     (resolve-directives opts directives)}))

(defn resolve-operation
  [opts operations operation-name]
  (->> (select-operation operations operation-name)
       (resolve-operation* opts)))
