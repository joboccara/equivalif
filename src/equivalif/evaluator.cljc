(ns equivalif.evaluator
  (:require [clojure.set :as set]
            [clojure.walk]
            [equivalif.ast-builder :as ast]
            [equivalif.platform :as platform]))

(declare boolean-symbol? generate-combinations find-vars find-vars-in-ast)

(def inject clojure.walk/prewalk-replace)

(defn truth-table-from-ast
  [ast]
  (let [vars (find-vars-in-ast ast)]
    (for [combination (generate-combinations vars [false true])]
      (let [variables-map (apply hash-map combination)]
        {:variables variables-map
         :result (platform/p-eval (inject (merge variables-map platform/boolean-operators-map) ast))}))))

(defn generate-combinations [xs ys]
  (reduce (fn [combinations x]
            (mapcat #(for [y ys] (vec (concat % [x y]))) combinations))
          [[]] xs))

(defn find-vars-in-ast
  [ast]
  (cond
    (boolean-symbol? ast) #{}
    (symbol? ast) #{ast}
    (coll? ast) (apply set/union (map find-vars-in-ast ast))
    :else nil))

(defn boolean-symbol?
  [symb]
  (some #(= % symb) ['and 'or 'not]))

(def find-vars (comp find-vars-in-ast ast/parse))
(def truth-table (comp truth-table-from-ast ast/parse))
