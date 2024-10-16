(ns equivalif.truth-table
  (:require [clojure.set :as set]
            [equivalif.ast-builder :as ast]
            [equivalif.evaluator :as evaluator]))

(declare boolean-operator? generate-combinations find-vars find-vars-in-ast)

(defn truth-table-from-ast [ast]
  (let [vars (find-vars-in-ast ast)]
    (for [combination (generate-combinations vars [false true])]
      (let [variables-map (apply hash-map combination)]
        {:variables variables-map
         :result (evaluator/evaluate ast variables-map)}))))

(defn generate-combinations [xs ys]
  (reduce (fn [combinations x]
            (mapcat #(for [y ys] (vec (concat % [x y]))) combinations))
          [[]] xs))

(defn find-unsorted-vars-in-ast
  ([ast] (find-unsorted-vars-in-ast ast false))
  ([ast if-block?]
   (cond
    (boolean-operator? ast) #{}
    (symbol? ast) (if if-block? nil #{ast})
    (and (coll? ast) (= (first ast) 'if)) (apply set/union (cons (find-unsorted-vars-in-ast (second ast) false)
                                                                 (map #(find-unsorted-vars-in-ast % true) (drop 2 ast))))
    (coll? ast) (apply set/union (map #(find-unsorted-vars-in-ast % false) ast))
    :else #{})))

(def find-vars-in-ast (comp sort find-unsorted-vars-in-ast))

(defn boolean-operator?  [symb]
  (some #(= % symb) ['and 'or 'not 'if]))

(def find-vars (comp find-vars-in-ast ast/parse))
(def truth-table (comp truth-table-from-ast ast/parse))
