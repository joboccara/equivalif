(ns equivalif.ast-builder
  (:require [equivalif.lexer :refer :all]))

(declare token-to-symbol)

(defn ast
  ([tokens] (ast [()] tokens))
  ([stack tokens]
  (if (empty? tokens) (last stack)
  (recur (into (pop stack) [(cons (token-to-symbol (first tokens)) (last stack))])
         (rest tokens)))))

(defn token-to-symbol
  [token]
  (println token)
  (symbol (:name token)))

(def parse (comp ast lex))