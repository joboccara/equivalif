(ns equivalif.lexer
  (:require [clojure.string :as string]))

(declare to-token)

(defn lex
  "Transforms source code into a list of tokens"
  [source]
  (if (empty? source)
    []
    (map to-token (string/split source #" "))))

(defn to-token
  "Transforms a string symbol into a token"
  [symbol]
  (cond
    (= symbol "&&") {:type :and}
    (= symbol "||") {:type :or}
    :else {:type :variable, :name symbol}))
