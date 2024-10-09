(ns equivalif.ast-builder
  (:require [equivalif.lexer :as l]
            [equivalif.platform :as platform]))

(declare add-token-to-stack addable-to-stack ast-infix balanced? infix-operator? trim-parens token-to-symbol)

(defn infix-to-prefix [ast]
  (if (symbol? ast) ast ; else ast is a collection
  (if (infix-operator? (second ast))
    [(second ast) (infix-to-prefix (first ast)) (infix-to-prefix (last ast))]
    (map infix-to-prefix ast))))

(defn infix-operator?  [symb]
  (some #(= % symb) ['and 'or]))

(def invalid-expression '())

(defn ast-infix
  ([tokens] (ast-infix [[]] tokens))
  ([stack tokens]
  (cond (empty? tokens) (if (balanced? stack) (last stack) invalid-expression)
        (not (addable-to-stack stack (first tokens))) invalid-expression
        :else (recur (add-token-to-stack stack (first tokens)) (rest tokens)))))

(defn add-token-to-stack [stack token]
  (let [open? (= :open (:type token))
        close? (= :close (:type token))]
  (cond
    open? (conj stack [])
    close? (conj (pop (pop stack)) (conj (last (pop stack)) (last stack)))
    :else (conj (pop stack) (conj (last stack) (token-to-symbol token))))))

(defn addable-to-stack [stack token]
  (not (and (= :close (:type token)) (<= (count stack) 1))))

(defn token-to-symbol [token]
  (cond
    (= :and (:type token)) 'and
    (= :or (:type token)) 'or
    (= :not (:type token)) 'not
    :else `(identity ~(symbol (:name token)))))

(defn balanced?  [stack]
  (= 1 (count stack)))

(defn add-parens-for-not-precedence-in-list
  ([ast]
   (if (symbol? ast) ast
     (add-parens-for-not-precedence-in-list ast (keep-indexed #(when (= %2 'not) %1) ast))))
  ([ast not-positions]
   (if (empty? not-positions) ast
       (let [not-position (last not-positions)]
       (if (>= (+ 1 not-position) (count ast)) invalid-expression
         (recur (concat
                 (take not-position ast)
                 (list (list 'not (nth ast (+ 1 not-position))))
                 (drop (+ 2 not-position) ast))
                (butlast not-positions)))))))

(defn add-parens-for-operator-precedence-in-list
  ([operator ast]
   (if (symbol? ast) ast
     (add-parens-for-operator-precedence-in-list operator ast (keep-indexed #(when (= %2 operator) %1) ast))))
  ([operator ast positions]
   (if (empty? positions) ast
       (let [position (last positions)]
       (if (or (< (- position 1) 0) (>= (+ position 1) (count ast))) invalid-expression
         (recur operator
                (concat
                 (take (- position 1) ast)
                 (list (list (nth ast (- position 1)) operator (nth ast (+ position 1))))
                 (drop (+ position 2) ast))
                (butlast positions)))))))

(defn add-parens-for-precedence-in-list [ast]
  (add-parens-for-operator-precedence-in-list 'or (add-parens-for-operator-precedence-in-list 'and (add-parens-for-not-precedence-in-list ast))))

(defn add-parens-for-precedence
  "Applies add-parens-for-precedence-in-list recursively down the AST"
  [ast]
  (if (symbol? ast) ast
      (map add-parens-for-precedence (trim-parens (add-parens-for-precedence-in-list ast)))))

(defn arity [operator]
  (condp = operator
    'and 2
    'or 2
    'not 1
    platform/qualified-identity-symbol 1
    0))

(defn valid-infix-arity? [ast]
  (if (symbol? ast) true 
    (if (infix-operator? (second ast))
      (and (= (- (count ast) 1) (arity (second ast)))
           (every? valid-infix-arity? (cons (first ast) (drop 2 ast))))
      (and (= (count (rest ast)) (arity (first ast)))
           (every? valid-infix-arity? (rest ast))))))


(defn validate-infix-arity [ast]
  (if (valid-infix-arity? ast) ast invalid-expression))

(defn trim-parens [ast]
  (cond
    (symbol? ast) ast
    (= 1 (count ast)) (trim-parens (first ast))
    :else (map trim-parens ast)))

(defn deep-seq [ast]
  (if (coll? ast) (map deep-seq ast) ast))

(def ast #(-> %
          ast-infix
          add-parens-for-precedence
          trim-parens
          validate-infix-arity
          infix-to-prefix
          deep-seq))

(def parse (comp ast l/lex))
