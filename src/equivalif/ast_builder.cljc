(ns equivalif.ast-builder
  (:require [equivalif.lexer :as l]))

(declare add-token-to-stack ast-infix infix-operator? minimal-ast token-to-symbol)

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
  (if (empty? tokens) (minimal-ast (last stack))
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
    (= :not (:type token)) 'not
    :else `(identity ~(symbol (:name token)))))

(defn minimal-ast [ast]
  (cond
    (symbol? ast) ast
    (= 1 (count ast)) (minimal-ast (first ast))
    :else ast))

(defn deep-seq
  [ast]
  (if (coll? ast) (map deep-seq ast) ast))

(def ast (comp deep-seq infix-to-prefix ast-infix))
(def parse (comp ast l/lex))
