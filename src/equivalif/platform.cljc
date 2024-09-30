(ns equivalif.platform
  #?(:clj (:require [equivalif.platform-clj :as p-clj])
     :cljs (:require [equivalif.platform-cljs :as p-cljs])))

(def p-eval
  #?(:clj p-clj/p-eval
     :cljs p-cljs/p-eval))

(def boolean-operators-map
  #?(:clj p-clj/boolean-operators-map
     :cljs p-cljs/boolean-operators-map))