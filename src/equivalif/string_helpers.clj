(ns equivalif.string-helpers
  (:require [clojure.string]))

(defn split-keep-separator
  "Returns the separators and the substrings between the separators in their initial order"
  ([input regex] (split-keep-separator [] input regex))
  ([result input regex]
  (if (empty? input) result
  (let [matcher (re-matcher regex input)]
    (if (.find matcher)
      (let [chunk-end (if (= 0 (.start matcher)) (.end matcher) (.start matcher))]
        (recur (into result [(subs input 0 chunk-end)]) (subs input chunk-end) regex))
      (into result [input]))))))
