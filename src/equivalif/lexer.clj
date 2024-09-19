(ns equivalif.lexer
  (:require [clojure.string :as string]))

(defn lex
  "Transforms source code into a list of tokens"
  [source]
  (if (empty? source)
    []
    (map #(hash-map :type :variable :name %) (string/split source #" "))))