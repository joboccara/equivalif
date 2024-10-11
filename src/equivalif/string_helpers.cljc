(ns equivalif.string-helpers
  (:require [clojure.string]
            [equivalif.platform :as platform]))

(defn split-keep-separator
  "Returns the separators and the substrings between the separators in their initial order"
  ([input regex] (split-keep-separator [] input regex))
  ([result input regex]
  (if (empty? input) result
  (let [match (platform/match-regex regex input)]
    (if match
      (let [chunk-end (if (= 0 (:start match)) (:end match) (:start match))]
        (recur (into result [(subs input 0 chunk-end)]) (subs input chunk-end) regex))
      (into result [input]))))))
