(ns equivalif.platform
  #?(:clj (:require [equivalif.platform-clj :as p-clj])
     :cljs (:require [equivalif.platform-cljs :as p-cljs])))

(def match-regex
  #?(:clj p-clj/match-regex
     :cljs p-cljs/match-regex))