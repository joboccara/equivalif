(ns equivalif.ast-builder
  (:require [equivalif.collection-helpers :refer [keep-if-index remove-around-value remove-if-index]]
            [equivalif.lexer :as l]))

(declare add-token-to-stack addable-to-stack ast-infix balanced? boolean-infix-expression? infix-operator? trim-parens token-to-symbol valid-if-structure?)

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
    :else (conj (pop stack) (conj (last stack) (token-to-symbol token))))))

(defn closing-paren-not-matching-opening-paren? [stack token]
  (and (= :close (:type token)) (<= (count stack) 1)))

(defn else-block-without-else-keyword? [stack token]
  (and (= (first (last stack)) 'if) (not (contains? #{:else :else-if :close-block} (:type token))) (= (mod (count (last stack)) 3) 0)))

(defn empty-expression? [stack token]
  (and (= :close (:type token)) (empty? (last stack))))

(defn addable-to-stack [stack token]
  (and (not (closing-paren-not-matching-opening-paren? stack token))
       (not (else-block-without-else-keyword? stack token))
       (not (empty-expression? stack token))))

(defn token-to-symbol [token]
  (condp = (:type token)
    :and 'and
    :or 'or
    :not 'not
    :if 'if
    :else 'else
    :else-if 'else-if
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
    (= level :inner) (symbol (apply str (remove-around-value (interpose " " ast) " " (symbol "\n"))))
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

(defn valid-operator-arity? [operator arity]
  (condp = operator
    'and (= arity 2)
    'or (= arity 2)
    'not (= arity 1)
    'if (contains? #{1 2} (mod arity 3))
    (= arity 0)))

(defn valid-infix-arity? [ast]
  (if (symbol? ast) true 
    (if (infix-operator? (second ast))
      (and (valid-operator-arity? (second ast) (- (count ast) 1))
           (every? valid-infix-arity? (cons (first ast) (drop 2 ast))))
      (and (valid-operator-arity? (first ast) (count (rest ast)))
           (every? valid-infix-arity? (rest ast))))))

(defn valid-infix-operands? [ast]
  (if (symbol? ast) true
      (if (cond
            (= (first ast) 'not) (not (boolean-operator? (second ast)))
            (contains? #{'and 'or} (second ast)) (and (not (boolean-operator? (nth ast 0))) (not (boolean-operator? (nth ast 2))))
            (some (set '(if else-if else)) ast) (valid-if-structure? ast)
            :else false)
        (every? valid-infix-operands? ast)
        false)))

(defn valid-if-structure? [ast]
  (let [if-keywords (keep-if-index #(= (mod % 3) 0) ast)
        expressions (remove-if-index #(= (mod % 3) 0) ast)]
    (and (= (first if-keywords) 'if)
         (every? #(= % 'else-if) (drop-last (rest if-keywords)))
         (or (= (count ast) 3)
             (and (= (mod (count ast) 3) 2) (= (last if-keywords) 'else))
             (and (= (mod (count ast) 3) 0) (= (last if-keywords) 'else-if)))
         (not-any? boolean-operator? expressions))))

(defn validate-infix-arity [ast]
  (if (and (valid-infix-arity? ast) (valid-infix-operands? ast)) ast invalid-expression))

(defn trim-parens [ast]
  (cond
    (symbol? ast) ast
    (= 1 (count ast)) (trim-parens (first ast))
    :else (map trim-parens ast)))

(defn deep-seq [ast]
  (if (coll? ast) (map deep-seq ast) ast))

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
