(ns equivalif.evaluator
  (:require [clojure.walk]))

(declare has-else?)

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
         (= (nth ast 0) 'if) (cond (evaluate (nth ast 1)) (evaluate (nth ast 2))
                                   :else (if (has-else? ast) (evaluate (last ast)) nil))
         :else nil)))

(defn has-else? [ast]
  (and (= (mod (count ast) 3) 2) (= (nth ast (- (count ast) 2)) 'else)))