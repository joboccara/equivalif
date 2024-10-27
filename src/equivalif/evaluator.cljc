(ns equivalif.evaluator
  (:require [clojure.walk]
            [equivalif.collection-helpers :refer [remove-if-index]]))

(declare cond-fn conditions-and-blocks has-else? if-conditions-and-blocks)

(defn evaluate
  "injects the values of the map (symbol -> boolean) in expression and evaluates it"
  ([ast variables-map]
   (evaluate (clojure.walk/prewalk-replace variables-map ast)))
  ([ast]
   (cond (boolean? ast) ast
         (symbol? ast) (name ast)
         (= (nth ast 0) 'and) (and (evaluate (nth ast 1)) (evaluate (nth ast 2)))
         (= (nth ast 0) 'or) (or (evaluate (nth ast 1)) (evaluate (nth ast 2)))
         (= (nth ast 0) 'not) (not (evaluate (nth ast 1)))
         (= (nth ast 0) 'if) (apply cond-fn (concat
                                             (map evaluate (conditions-and-blocks ast))
                                             (list :else (if (has-else? ast) (evaluate (last ast)) nil))))
         :else nil)))

(defn conditions-and-blocks [ast]
  (remove-if-index #(= (mod % 3) 0) (if-conditions-and-blocks ast)))

(defn if-conditions-and-blocks [ast]
  (if (has-else? ast) (drop-last 2 ast) ast))

(defn has-else? [ast]
  (and (= (mod (count ast) 3) 2) (= (nth ast (- (count ast) 2)) 'else)))

(defn cond-fn [& clauses]
  (if (< (count clauses) 2) nil
      (if (first clauses) (second clauses) (recur (drop 2 clauses)))))