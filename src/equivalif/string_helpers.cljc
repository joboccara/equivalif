(ns equivalif.string-helpers
  (:require [clojure.string]))

(declare match-regex)

(defn split-keep-separator
  "Returns the separators and the substrings between the separators in their initial order"
  ([input regex] (split-keep-separator [] input regex))
  ([result input regex]
  (if (empty? input) result
  (let [match (match-regex regex input)]
    (if match
      (let [chunk-end (if (= 0 (:start match)) (:end match) (:start match))]
        (recur (into result [(subs input 0 chunk-end)]) (subs input chunk-end) regex))
      (into result [input]))))))


(defn match-regex [regex string]
  #?(:cljs
     (let [regex (js/RegExp. regex)]
       (let [match (.exec regex string)]
         (when match
           {:start (.index match)
            :end (+ (.index match) (count (first match)))})))
     :clj
     (let [matcher (re-matcher regex string)]
       (when (.find matcher)
         {:start (.start matcher)
          :end (.end matcher)}))))
