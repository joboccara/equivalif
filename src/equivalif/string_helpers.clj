(ns equivalif.string-helpers
  (:require [clojure.string]))

(defn split-keep-separator
  "Returns the separators and the substrings between the separators in their initial order"
  ([input regex] (split-keep-separator [] input regex))
  ([result input regex]
  (if (empty? input) result
  (let [matcher (re-matcher regex input)]
    (if (.find matcher)
      (let [chunks (into
                    (if (> (.start matcher) 0) [(subs input 0 (.start matcher))] [])
                    [(subs input (.start matcher) (.end matcher))])]
        (recur (into result chunks) (subs input (.end matcher)) regex))
      (into result [input]))))))
