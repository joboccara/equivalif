(ns equivalif.evaluator
  (:require [clojure.walk]))

(defn evaluate
  "injects the values of the map (symbol -> boolean) in expression and evaluates it"
  ([ast variables-map]
   (evaluate (clojure.walk/prewalk-replace variables-map ast)))
  ([ast]
   (cond (boolean? ast) ast
        (= (nth ast 0) 'and) (and (evaluate (nth ast 1)) (evaluate (nth ast 2)))
        (= (nth ast 0) 'or) (or (evaluate (nth ast 1)) (evaluate (nth ast 2)))
        (= (nth ast 0) 'not) (not (evaluate (nth ast 1)))
        :else nil)))
