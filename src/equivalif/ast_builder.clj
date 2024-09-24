(ns equivalif.ast-builder
  (:require [equivalif.lexer :refer :all]))

(declare add-token-to-stack ast-infix infix-operator? token-to-symbol)

(defn infix-to-prefix [ast]
  (if (symbol? ast) ast ; else ast is a collection
  (if (infix-operator? (second ast))
    [(second ast) (infix-to-prefix (first ast)) (infix-to-prefix (last ast))]
    (map infix-to-prefix ast))))

(defn infix-operator?
  [symb]
  (some #(= % symb) ['and 'or]))

(defn ast-infix
  ([tokens] (ast-infix [[]] tokens))
  ([stack tokens]
  (if (empty? tokens) (last stack)
  (recur (add-token-to-stack stack (first tokens)) (rest tokens)))))

(defn add-token-to-stack
  [stack token]
  (let [open? (= :open (:type token))
        close? (= :close (:type token))]
  (cond
    open? (conj stack [])
    close? (conj (pop (pop stack)) (conj (last (pop stack)) (last stack)))
    :else (conj (pop stack) (conj (last stack) (token-to-symbol token))))))

(defn token-to-symbol
  [token]
  (cond
    (= :and (:type token)) 'and
    (= :or (:type token)) 'or
    :else (symbol (:name token))))

(defn deep-seq
  [ast]
  (if (coll? ast) (map deep-seq ast) ast))

(def ast (comp deep-seq infix-to-prefix ast-infix))
(def parse (comp ast lex))
