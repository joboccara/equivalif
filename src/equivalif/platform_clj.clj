(ns equivalif.platform-clj)

(defn match-regex [regex string]
  (let [matcher (re-matcher regex string)]
    (when (.find matcher)
      {:start (.start matcher)
       :end (.end matcher)})))
