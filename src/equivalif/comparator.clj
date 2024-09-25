(ns equivalif.comparator
  (:require [equivalif.evaluator :refer :all]))

(declare comparable-truth-tables?)

(defn comparable?
  [expression1 expression2]
  (comparable-truth-tables? (truth-table expression1) (truth-table expression2)))

(defn comparable-truth-tables?
  [truth-table1 truth-table2]
  (cond
    (and (empty? truth-table1) (empty? truth-table2)) true
    (or (empty? truth-table1) (empty? truth-table2)) false
    :else (= (keys (:variables (first truth-table1))) (keys (:variables (first truth-table2))))))

(declare compare-variable-maps)

(defn truth-table-diff-from-truth-tables
  [truth-table1 truth-table2]
  (if (not (comparable-truth-tables? truth-table1 truth-table2)) (throw (ex-info "Truth tables are not comparable" {}))
  (remove #(= (:first %) (:second %))
          (map #(hash-map :variables (:variables %1) :first (:result %1) :second (:result %2))
               (sort-by :variables compare-variable-maps truth-table1)
               (sort-by :variables compare-variable-maps truth-table2)))))

(defn truth-table-diff
  [expression1 expression2]
  (truth-table-diff-from-truth-tables (truth-table expression1) (truth-table expression2)))

(defn compare-variable-maps
  "Assumes the same variables in both inputs"
  [variables-map1 variables-map2]
  (let [variables (sort (keys variables-map1))]
    (compare (mapv variables-map1 variables) (mapv variables-map2 variables))))
