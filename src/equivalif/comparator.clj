(ns equivalif.comparator)


(declare compare-variable-maps)

(defn truth-table-diff
  [truth-table1 truth-table2]
  (remove #(= (:first %) (:second %))
          (map #(hash-map :variables (:variables %1) :first (:result %1) :second (:result %2))
               (sort-by :variables compare-variable-maps truth-table1)
               (sort-by :variables compare-variable-maps truth-table2))))

(defn compare-variable-maps
  "Assumes the same variables in both inputs"
  [variables-map1 variables-map2]
  (let [variables (sort (keys variables-map1))]
    (compare (mapv variables-map1 variables) (mapv variables-map2 variables))))
