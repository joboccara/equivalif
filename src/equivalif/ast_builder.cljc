(ns equivalif.ast-builder
  (:require [equivalif.lexer :as l]))

(declare add-token-to-stack addable-to-stack ast-infix balanced? infix-operator? trim-redundant-external-parens token-to-symbol)

(defn infix-to-prefix [ast]
  (if (symbol? ast) ast ; else ast is a collection
  (if (infix-operator? (second ast))
    [(second ast) (infix-to-prefix (first ast)) (infix-to-prefix (last ast))]
    (map infix-to-prefix ast))))

(defn infix-operator?
  [symb]
  (some #(= % symb) ['and 'or]))

(def invalid-expression '())

(defn ast-infix
  ([tokens] (ast-infix [[]] tokens))
  ([stack tokens]
  (cond (empty? tokens) (if (balanced? stack) (trim-redundant-external-parens (last stack)) invalid-expression)
        (not (addable-to-stack stack (first tokens))) invalid-expression
        :else (recur (add-token-to-stack stack (first tokens)) (rest tokens)))))

(defn add-token-to-stack
  [stack token]
  (let [open? (= :open (:type token))
        close? (= :close (:type token))]
  (cond
    open? (conj stack [])
    close? (conj (pop (pop stack)) (conj (last (pop stack)) (last stack)))
    :else (conj (pop stack) (conj (last stack) (token-to-symbol token))))))

(defn addable-to-stack
  [stack token]
  (not (and (= :close (:type token)) (<= (count stack) 1))))

(defn token-to-symbol
  [token]
  (cond
    (= :and (:type token)) 'and
    (= :or (:type token)) 'or
    (= :not (:type token)) 'not
    :else `(identity ~(symbol (:name token)))))

(defn balanced?
  [stack]
  (= 1 (count stack)))

(defn trim-redundant-external-parens [ast]
  (if (= 1 (count ast)) (trim-redundant-external-parens (first ast)) ast))

(defn deep-seq
  [ast]
  (if (coll? ast) (map deep-seq ast) ast))

(def ast (comp deep-seq infix-to-prefix ast-infix))
(def parse (comp ast l/lex))
