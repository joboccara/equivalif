(ns equivalif.platform
  #?(:clj (:require [equivalif.platform-clj :as p-clj])
     :cljs (:require [equivalif.platform-cljs :as p-cljs])))

(def qualified-identity-symbol
  #?(:clj p-clj/qualified-identity-symbol
     :cljs p-cljs/qualified-identity-symbol))
