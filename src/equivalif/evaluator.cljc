(ns equivalif.evaluator
  (:require [clojure.set :as set]
            [clojure.walk]
            [cljs.js :as cljs]
            [equivalif.ast-builder :as ast]))

(declare boolean-symbol? generate-combinations find-vars find-vars-in-ast)

(def st (cljs/empty-state)) 

(defn eval-cljs [expr]
(let [result (atom nil)]
  (cljs/eval st expr
             {:eval cljs/js-eval}
             (fn [value]
               (reset! result value)))
  @result))

(def platform-eval
  #?(:clj eval
     :cljs eval-cljs))

(defn non-macro-and [& args]
  (reduce #(and %1 %2) true args))

(defn non-macro-or [& args]
  (reduce #(or %1 %2) false args))

(def platform-boolean-operators-map
  #?(:clj {}
     :cljs {'and equivalif.evaluator/non-macro-and, 'or equivalif.evaluator/non-macro-or, 'not cljs.core/not}))

(def inject clojure.walk/prewalk-replace)

(defn truth-table-from-ast
  [ast]
  (let [vars (find-vars-in-ast ast)]
    (for [combination (generate-combinations vars [false true])]
      (let [variables-map (apply hash-map combination)]
        {:variables variables-map
         :result (platform-eval (inject (merge variables-map platform-boolean-operators-map) ast))}))))

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
