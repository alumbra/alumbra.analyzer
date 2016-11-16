(ns alumbra.canonical.fragments
  (:require [alumbra.canonical.selection-set
             :refer [resolve-fragment-selection-set]]
            [com.stuartsierra.dependency :as dep]
            [com.rpl.specter :refer :all]))

;; ## Topological Sort of Fragments

(def all-fragment-dependencies
  (comp-paths
    (recursive-path
      []
      p
      (cond-path
        :alumbra/selection-set
        [:alumbra/selection-set
         ALL
         (multi-path
           (must :alumbra/fragment-name)
           p)]
        STAY))))

(defn- add-fragment-dependencies
  [graph {:keys [alumbra/fragment-name] :as fragment}]
  (->> (traverse all-fragment-dependencies fragment)
       (reduce
         #(dep/depend %1 fragment-name %2)
         (dep/depend graph ::root fragment-name))))

(defn- sort-fragments
  "Sort fragments topologically."
  [fragments]
  (loop [graph     (dep/graph)
         result    {}
         fragments fragments]
    (if (seq fragments)
      (let [[{:keys [alumbra/fragment-name] :as fragment} & rst] fragments]
        (recur
          (add-fragment-dependencies graph fragment)
          (assoc result fragment-name fragment)
          rst))
      (->> (dep/topo-sort graph)
           (butlast)
           (map #(get result %))))))

;; ## Resolve Fragments

(defn- resolve-fragment
  [opts {:keys [alumbra/fragment-name] :as fragment}]
  (assoc-in opts
            [:fragments fragment-name]
            (resolve-fragment-selection-set opts fragment)))

(defn resolve-fragments
  "Resolve fragments, inlining fields from all dependent fragments directly
   into them. Produces a map associating fragment names with the canonical
   selection set for that fragment."
  [opts fragments]
  (->> (sort-fragments fragments)
       (reduce resolve-fragment opts)))
