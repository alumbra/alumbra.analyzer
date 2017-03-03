(ns alumbra.canonical.operations
  (:require [alumbra.canonical
             [selection-set :refer [resolve-selection-set]]
             [directives :refer [resolve-directives]]]))

;; ## Helpers

(defn- root-type
  [{{:keys [schema-root]} :schema}
   {:keys [alumbra/operation-type]}]
  (let [operation-type (or operation-type "query")]
    (or (get-in schema-root [:schema-root-types operation-type])
        (throw
          (IllegalArgumentException.
            (format "no root defined for '%s' operations; cannot canonicalize."
                    operation-type))))))

;; ## Operation Resolution

(defn select-operation
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

(defn resolve-operation
  [opts {:keys [alumbra/selection-set
                alumbra/directives
                alumbra/operation-type
                alumbra/operation-name] :as op}]
  (let [opts (-> opts
                 (assoc :scope-type (root-type opts op)))
        selection (resolve-selection-set opts selection-set)]
    {:operation-name operation-name
     :operation-type operation-type
     :selection-set  selection
     :directives     (resolve-directives opts directives)}))
