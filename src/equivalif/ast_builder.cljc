(ns equivalif.ast-builder
  (:require [equivalif.lexer :as l]))

(declare add-token-to-stack addable-to-stack ast-infix balanced? boolean-infix-expression? infix-operator? trim-parens token-to-symbol)

(defn infix-to-prefix [ast]
  (if (symbol? ast) ast ; else ast is a collection
  (if (infix-operator? (second ast))
    [(second ast) (infix-to-prefix (first ast)) (infix-to-prefix (last ast))]
    (map infix-to-prefix ast))))

(defn infix-operator?  [symb]
  (some #(= % symb) ['and 'or]))

(defn prefix-operator? [symb]
  (some #(= % symb) ['not 'if]))

(defn boolean-operator? [symb]
  (or (infix-operator? symb) (prefix-operator? symb)))

(def invalid-expression '())

(defn ast-infix
  ([tokens] (ast-infix [[]] tokens))
  ([stack tokens]
  (cond (empty? tokens) (if (balanced? stack) (last stack) invalid-expression)
        (not (addable-to-stack stack (first tokens))) invalid-expression
        :else (recur (add-token-to-stack stack (first tokens)) (rest tokens)))))

(defn add-token-to-stack [stack token]
  (let [open? (contains? #{:open :open-block} (:type token))
        close? (contains? #{:close :close-block} (:type token))]
  (cond
    open? (conj stack [])
    close? (conj (pop (pop stack)) (conj (last (pop stack)) (last stack)))
    (and (= (:type token) :else) (= (first (last stack)) 'if)) stack
    :else (conj (pop stack) (conj (last stack) (token-to-symbol token))))))

(defn addable-to-stack [stack token]
  (not (and (= :close (:type token)) (<= (count stack) 1))))

(defn token-to-symbol [token]
  (condp = (:type token)
    :and 'and
    :or 'or
    :not 'not
    :if 'if
    (symbol (:name token))))

(defn balanced?  [stack]
  (= 1 (count stack)))

(defn custom-expressions-to-symbols
  ([ast] (custom-expressions-to-symbols :outer ast))
  ([level ast]
   (cond
    (= ast invalid-expression) invalid-expression
    (and (boolean-operator? ast) (= level :outer)) invalid-expression
    (symbol? ast) ast
    (boolean-infix-expression? ast) (map #(custom-expressions-to-symbols :inner %) ast)
    (= level :inner) (symbol (apply str (interpose " " ast)))
    :else invalid-expression)))

(defn boolean-infix-expression? [ast]
  (or (prefix-operator? (first ast)) (infix-operator? (second ast))))

(defn no-nil-list [& values]
  (remove nil? (apply list values)))

(defn add-parens-for-operator-precedence-in-list [operator]
  (fn
    ([ast]
     (if (symbol? ast) ast
         ((add-parens-for-operator-precedence-in-list operator) ast (keep-indexed #(when (= %2 operator) %1) ast))))
    ([ast positions]
     (if (empty? positions) ast
         (let [infix? (infix-operator? operator)
               begin-position (- (last positions) (if infix? 1 0))
               end-position (+ (last positions) 1)]
           (if (or (< begin-position 0) (>= end-position (count ast))) invalid-expression
               (recur (concat
                       (take begin-position ast)
                       (list (no-nil-list (when infix? (nth ast begin-position)) operator (nth ast end-position)))
                       (drop (+ end-position 1) ast))
                      (butlast positions))))))))

(def add-parens-for-precedence-in-list
  #(-> %
       ((add-parens-for-operator-precedence-in-list 'not))
       ((add-parens-for-operator-precedence-in-list 'and))
       ((add-parens-for-operator-precedence-in-list 'or))
       ))

(defn add-parens-for-precedence
  "Applies add-parens-for-precedence-in-list recursively down the AST"
  [ast]
  (let [current-level-with-precedence (trim-parens (add-parens-for-precedence-in-list ast))]
    (if (symbol? current-level-with-precedence) current-level-with-precedence
        (map add-parens-for-precedence current-level-with-precedence))))

(defn arity [operator]
  (condp = operator
    'and 2
    'or 2
    'not 1
    'if 3
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

(defn tee [value] (println "tee" value) value)

(def ast #(-> %
          ast-infix
          trim-parens
          custom-expressions-to-symbols
          add-parens-for-precedence
          trim-parens
          validate-infix-arity
          infix-to-prefix
          deep-seq))

(def parse (comp ast l/lex))
