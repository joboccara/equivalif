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
     (and (coll? ast) (contains? #{'if 'else-if} (first ast))) (set/union (find-unsorted-vars-in-ast (nth ast 1) false)
                                                                          (find-unsorted-vars-in-ast (nth ast 2) true)
                                                                          (find-unsorted-vars-in-ast (drop 3 ast) false))
     (and (coll? ast) (= 'else (first ast))) (find-unsorted-vars-in-ast (second ast) true)
     (coll? ast) (apply set/union (map #(find-unsorted-vars-in-ast % false) ast))
     :else #{})))

(def find-vars-in-ast (comp sort find-unsorted-vars-in-ast))

(defn boolean-operator?  [symb]
  (some #(= % symb) ['and 'or 'not 'if]))

(def find-vars (comp find-vars-in-ast ast/parse))
(def truth-table (comp truth-table-from-ast ast/parse))
