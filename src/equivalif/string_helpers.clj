(ns equivalif.string-helpers
  (:require [clojure.string]
             [equivalif.array-helpers :as array-helpers]))

(defn split-keep-separator
   "Returns the separators and the substrings between the separators in their initial order"
  [input regex]
  (let [separators (re-seq regex input)
        substrings (remove empty? (clojure.string/split input regex))]
    (apply array-helpers/alternate
      (if (or (empty? substrings) (clojure.string/starts-with? input (first substrings)))
        [substrings separators]
        [separators substrings])))
  )
