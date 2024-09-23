(ns equivalif.ast-builder
  (:require [equivalif.lexer :refer :all]))

(declare ast-infix infix-operator? token-to-symbol)

(defn infix-to-prefix [ast]
  (if (symbol? ast) ast ; else ast is a collection
  (if (infix-operator? (second ast))
    [(second ast) (infix-to-prefix (first ast)) (infix-to-prefix (last ast))]
    (map infix-to-prefix ast))))

(defn infix-operator?
  [symb]
  (some #(= % symb) ['and]))

(defn ast-infix
  ([tokens] (ast-infix [[]] tokens))
  ([stack tokens]
  (if (empty? tokens) (last stack)
  (recur (into (pop stack) [(conj (last stack) (token-to-symbol (first tokens)))])
         (rest tokens)))))

(defn token-to-symbol
  [token]
  (cond
    (= :and (:type token)) 'and
    :else (symbol (:name token))))

(def ast (comp infix-to-prefix ast-infix))
(def parse (comp ast lex))
