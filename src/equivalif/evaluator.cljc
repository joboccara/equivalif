(ns equivalif.evaluator
  (:require [clojure.set :as set]
            [equivalif.ast-builder :as ast]))

(declare boolean-symbol? cartesian-product find-vars find-vars-in-ast)

(defn truth-table-from-ast
  [ast]
  (let [vars (find-vars-in-ast ast)
        keys (map keyword vars)]
    (eval `(cartesian-product ~vars [false true]
     {:variables (zipmap [~@keys] [~@vars])
      :result (eval ~ast)}))))

(defmacro cartesian-product
  [vars values & body]
  (let [bindings (interleave vars (repeat (count vars) values))]
    `(for [~@bindings] ~@body)))

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
