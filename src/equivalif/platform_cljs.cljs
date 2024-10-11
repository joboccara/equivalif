(ns equivalif.platform-cljs)

(defn match-regex [regex string]
  (let [regex (js/RegExp. regex)
        match (.exec regex string)]
    (when match
      {:start (.-index match)
       :end (+ (.-index match) (count (first match)))})))