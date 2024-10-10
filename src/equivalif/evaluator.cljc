(ns equivalif.evaluator)

(defn evaluate
  "injects the values of the map (symbol -> boolean value) in boolean expression and evaluates it"
  [ast values-map]
  (cond (symbol? ast) (ast values-map)
        (= (nth ast 0) 'and) (and (evaluate (nth ast 1) values-map) (evaluate (nth ast 2) values-map))
        (= (nth ast 0) 'or) (or (evaluate (nth ast 1) values-map) (evaluate (nth ast 2) values-map))
        (= (nth ast 0) 'not) (not (evaluate (nth ast 1) values-map))
        :else nil))
