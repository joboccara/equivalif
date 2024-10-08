(ns equivalif.platform-cljs
  (:require [cljs.js :as cljs]))

(def st (cljs/empty-state)) 

(defn eval-cljs [expr]
(let [result (atom nil)]
  (cljs/eval st expr
             {:eval cljs/js-eval}
             (fn [value]
               (reset! result (:value value))))
  @result))

(def p-eval eval-cljs)

(defn non-macro-and [& args]
  (reduce #(and %1 %2) true args))

(defn non-macro-or [& args]
  (reduce #(or %1 %2) false args))

(def boolean-operators-map
  {'and equivalif.platform-cljs/non-macro-and, 'or equivalif.platform-cljs/non-macro-or, 'not cljs.core/not, 'cljs.core/identity identity})
