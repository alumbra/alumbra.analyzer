(ns alumbra.canonical.operations
  (:require [alumbra.canonical.selection-set
             :refer [resolve-selection-set]]
            [alumbra.canonical.directives
             :refer [resolve-directives]]))

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
  (let [root-type (root-type opts op)
        selection (resolve-selection-set
                    (assoc opts :scope-type root-type)
                    selection-set)]
    {:operation-name operation-name
     :operation-type operation-type
     :selection-set  selection
     :directives     (resolve-directives opts directives)}))

(defn resolve-operation
  [opts operations operation-name]
  (->> (select-operation operations operation-name)
       (resolve-operation* opts)))
