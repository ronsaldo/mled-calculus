#lang racket

(require racket/match)
(require rackunit)
(require "util-graphviz.rkt")

;; AST Syntactic nodes.
(struct stx-integer (value) #:transparent)
(struct stx-true () #:transparent)
(struct stx-false () #:transparent)
(struct stx-unit () #:transparent)
(struct stx-application (functional argument) #:transparent)
(struct stx-lambda (argument body) #:transparent)
(struct stx-epsilon (argument body) #:transparent)
(struct stx-expand (argument body) #:transparent)
(struct stx-mu (argument body) #:transparent)
(struct stx-let (name value body) #:transparent)
(struct stx-if (condition true-expression false-expression) #:transparent)
(struct stx-identifier (symbol) #:transparent)
(struct stx-pair (first second) #:transparent)
(struct stx-first (pair) #:transparent)
(struct stx-second (second) #:transparent)
(struct stx-inject-left (expression) #:transparent)
(struct stx-inject-right (expression) #:transparent)
(struct stx-case (expression left right) #:transparent)
(struct val-symbol (symbol) #:transparent)

;; parse-sexpr :: String -> Syntax
;; Syntax Grammar:
;; expr ::= <integer>
;;        | <boolean>
;;        | <identifier>
;;        | ()
;;        | (if <expr> <expr> <expr>)
;;        | (lambda (<identifier>*) <expr>)
;;        | (mu (<identifier>*) <expr>)
;;        | (epsilon (<identifier>*) <expr>)
;;        | (let ( (<identifier> <expr>)+ ) <expr>)
;;
;;        | (pair <expr> <expr>)
;;        | (first <expr>)
;;        | (second <expr>)
;;
;;        | (inject-left <expr>)
;;        | (inject-right <expr>)
;;        | (case <expr>)
;;
;;        | (expand <expr> <expr>+)
;;        | (<expr> <expr>+)
;; Notes:
;; - Lambdas with multiple arguments are currified.
;; - Let expression with multiple arguments are normalized in a similar way to currified lambdas.
(define (parse-sexpr sexpr)
  (cond
    [(integer? sexpr) (stx-integer sexpr)]
    [(boolean? sexpr) (if sexpr (stx-true) (stx-false))]
    [(symbol? sexpr) (stx-identifier sexpr)]
    [(list? sexpr) [match sexpr
      ([list] (stx-unit))
      ([list 'if condition true-expression false-expression]
        (stx-if (parse-sexpr condition) (parse-sexpr true-expression) (parse-sexpr false-expression)))
      ([list 'not expression]
        (stx-if (parse-sexpr expression) (stx-false) (stx-true)))
      ([list 'epsilon arguments body] (foldr stx-epsilon (parse-sexpr body) (map val-symbol arguments)))
      ([list-rest 'expand macro arguments] (foldl (lambda (a f) (stx-expand f a)) (parse-sexpr macro) (map parse-sexpr arguments)))
      ([list 'lambda arguments body] (foldr stx-lambda (parse-sexpr body) (map val-symbol arguments)))
      ([list 'mu arguments body] (foldr stx-mu (parse-sexpr body) (map val-symbol arguments)))
      ([list 'let nameValues body]
        (foldr (lambda (nameValuePair body)
          (stx-let (val-symbol (first nameValuePair)) (parse-sexpr (second nameValuePair)) body)
        ) (parse-sexpr body) nameValues))

      ;; Products
      ([list 'pair first second] (stx-pair (parse-sexpr first) (parse-sexpr second)))
      ([list 'first pair] (stx-first (parse-sexpr pair)))
      ([list 'second pair] (stx-second (parse-sexpr pair)))

      ;; Sums
      ([list 'inject-left expr] (stx-inject-left (parse-sexpr expr)))
      ([list 'inject-right expr] (stx-inject-right (parse-sexpr expr)))
      ([list 'case expr left right] (stx-case (parse-sexpr expr) (parse-sexpr left) (parse-sexpr right)))

      ;; Remaining case, applications.
      ([list-rest functional arguments] (foldl (lambda (a f) (stx-application f a)) (parse-sexpr functional) (map parse-sexpr arguments)))
    ]]
    [else (error "Unexpected syntax" sexpr)]))

(test-case
  "Parse SExpr"
  (check-equal? (parse-sexpr 1) (stx-integer 1))
  (check-equal? (parse-sexpr #t) (stx-true))
  (check-equal? (parse-sexpr #f) (stx-false))
  (check-equal? (parse-sexpr 'test) (stx-identifier 'test))
  (check-equal? (parse-sexpr '(x 42)) (stx-application (stx-identifier 'x) (stx-integer 42)))
  (check-equal? (parse-sexpr '(x 42 45)) (stx-application (stx-application (stx-identifier 'x) (stx-integer 42)) (stx-integer 45)))
  (check-equal? (parse-sexpr '(expand x 42)) (stx-expand (stx-identifier 'x) (stx-integer 42)))
  (check-equal? (parse-sexpr '(expand x 42 45)) (stx-expand (stx-expand (stx-identifier 'x) (stx-integer 42)) (stx-integer 45)))
  (check-equal? (parse-sexpr '()) (stx-unit))
  (check-equal? (parse-sexpr '(if #t 1 2)) (stx-if (stx-true) (stx-integer 1) (stx-integer 2)))
  (check-equal? (parse-sexpr '(not #t)) (stx-if (stx-true) (stx-false) (stx-true)))
  (check-equal? (parse-sexpr '(epsilon (x) x)) (stx-epsilon (val-symbol 'x) (stx-identifier 'x)))
  (check-equal? (parse-sexpr '(epsilon (x y) x)) (stx-epsilon (val-symbol 'x) (stx-epsilon (val-symbol 'y) (stx-identifier 'x))))
  (check-equal? (parse-sexpr '(lambda (x) x)) (stx-lambda (val-symbol 'x) (stx-identifier 'x)))
  (check-equal? (parse-sexpr '(lambda (x y) x)) (stx-lambda (val-symbol 'x) (stx-lambda (val-symbol 'y) (stx-identifier 'x))))
  (check-equal? (parse-sexpr '(mu (x) x)) (stx-mu (val-symbol 'x) (stx-identifier 'x)))
  (check-equal? (parse-sexpr '(mu (x y) x)) (stx-mu (val-symbol 'x) (stx-mu (val-symbol 'y) (stx-identifier 'x))))
  (check-equal? (parse-sexpr '(let [(x 42)] x)) (stx-let (val-symbol 'x) (stx-integer 42) (stx-identifier 'x)))
  (check-equal? (parse-sexpr '(let [(x 42) (y 45)] y)) (stx-let (val-symbol 'x) (stx-integer 42) (stx-let (val-symbol 'y) (stx-integer 45) (stx-identifier 'y))))
  (check-equal? (parse-sexpr '(pair 1 2)) (stx-pair (stx-integer 1) (stx-integer 2)))
  (check-equal? (parse-sexpr '(first x)) (stx-first (stx-identifier 'x)))
  (check-equal? (parse-sexpr '(second x)) (stx-second (stx-identifier 'x)))
  (check-equal? (parse-sexpr '(inject-left 42)) (stx-inject-left (stx-integer '42)))
  (check-equal? (parse-sexpr '(inject-right 42)) (stx-inject-right (stx-integer '42)))
  (check-equal? (parse-sexpr '(case x y z)) (stx-case (stx-identifier 'x) (stx-identifier 'y) (stx-identifier 'z)) )
)

;; Environment :: Empty | Environment (Symbol -> Value)
;; Compiler Value ::
;;                 | VApply(Value, Value)
;;                 | VArgument
;;                 | VIf(Value, Value, Value) 
;;                 | VInteger | VTrue | VFalse
;;                 | VLambda(VArgument, Value)
;;                 | VDict(Value, Value, Value)
;;                 | VDictLookup(Value, Value)
;;                 | VEpsilon(VEpsilonArgument, Value)
;;                 | VEpsilonArgument
;;                 | VExpand
;;                 | VMu(VMuArgument, Value)
;;                 | VMuArgument
;;                 | VPrimitive(Value -> Value)
;;                 | VUnit
;;                 | VPair(Value, Value)
;;                 | VFirst(Pair)
;;                 | VSecond(Pair)
;;                 | VInjectLeft(Value)
;;                 | VInjectRight(Value)
;;                 | VCase(Value, Value, Value)
;;                 | VSymbol(Value)
;;                 | VDict(Parent, Value, Value)
(struct val-apply (functional argument) #:transparent)
(struct val-argument () #:transparent)
(struct val-if (condition true-block false-block) #:transparent)
(struct val-integer (value) #:transparent)
(struct val-true () #:transparent)
(struct val-false () #:transparent)
(struct val-lambda (argument body) #:transparent)
(struct val-dict (parent symbol value) #:transparent)
(struct val-dict-lookup (dict symbol) #:transparent)
(struct val-eps (argument body) #:transparent)
(struct val-epsargument () #:transparent)
(struct val-expand (syntax argument) #:transparent)
(struct val-mu (argument body) #:transparent)
(struct val-muargument () #:transparent)
(struct val-pair (first second) #:transparent)
(struct val-first (pair) #:transparent)
(struct val-second (pair) #:transparent)
(struct val-primitive (implementation) #:transparent)
(struct val-unit () #:transparent)
(struct val-inject-left (value) #:transparent)
(struct val-inject-right (value) #:transparent)
(struct val-case (value left-case right-case) #:transparent)

(define val-environment-empty-arg (val-muargument))
(define val-environment-empty (val-mu val-environment-empty-arg val-environment-empty-arg))

;; val-unwrap-boolean :: VTrue | VFalse -> Boolean
(define (val-unwrap-boolean value)
  (match value
    [(val-true) #t]
    [(val-false) #f]))

;; val-wrap-boolean :: Boolean -> VTrue | VFalse
(define (val-wrap-boolean value)
  (if value
    (val-true)
    (val-false)))

;; val-primitive-integer-unary-operation :: (Integer -> Integer) -> val-primitive
(define (val-primitive-integer-unary-operation operation)
  (val-primitive (lambda (argument)
    (define argument-value (val-integer-value argument))
    (val-integer (operation argument-value)))))

;; val-primitive-integer-binary-operation :: (Integer -> Integer ->  Integer) -> val-primitive
(define (val-primitive-integer-binary-operation binop)
  (val-primitive (lambda (left)
    (define left-value (val-integer-value left))
    (val-primitive (lambda (right)
      (define right-value (val-integer-value right))
        (val-integer (binop left-value right-value)))))))

;; val-primitive-integer-comparison :: (Integer -> Integer -> Boolean) -> val-primitive
(define (val-primitive-integer-comparison comparison)
  (val-primitive (lambda (left)
    (define left-value (val-integer-value left))
    (val-primitive (lambda (right)
      (define right-value (val-integer-value right))
        (val-wrap-boolean (comparison left-value right-value)))))))

(test-case
  "Primitives"
  [check-equal?
    ((val-primitive-implementation (val-primitive-integer-unary-operation -)) (val-integer 2))
    (val-integer -2)]
  [check-equal?
    ((val-primitive-implementation ((val-primitive-implementation (val-primitive-integer-binary-operation +)) (val-integer 2))) (val-integer 3))
    (val-integer 5)]
  [check-equal?
    ((val-primitive-implementation ((val-primitive-implementation (val-primitive-integer-comparison <)) (val-integer 2))) (val-integer 3))
    (val-true)]
  [check-equal?
    ((val-primitive-implementation ((val-primitive-implementation (val-primitive-integer-comparison <)) (val-integer 3))) (val-integer 2))
    (val-false)]
)

;; environment-with-values-expansion :: Environment, List(Symbol, Value) -> Environment
(define (environment-with-values-expansion environment symbols-with-values)
  (match symbols-with-values
    [(list) environment]
    [(list-rest (list symbol value) rest)
      (environment-with-values-expansion (val-dict environment symbol value) rest)]))

;; environment-with-values :: Environment, List(Symbol, Value) -> Environment
(define-syntax environment-with-values
  (syntax-rules ()
    [(_ base-environment ([symbol value]...))
      (environment-with-values-expansion base-environment (list (list (val-symbol (quote symbol)) value) ...))
    ]))

(test-case
  "environment-with-values"
  [check-equal?
    (environment-with-values val-environment-empty [])
    val-environment-empty]
  [check-equal?
    (environment-with-values val-environment-empty [(true (val-true))])
    (val-dict val-environment-empty (val-symbol 'true) (val-true))]
  [check-equal?
    (environment-with-values val-environment-empty [(true (val-true)) (false (val-false))])
    (val-dict (val-dict val-environment-empty (val-symbol 'true) (val-true)) (val-symbol 'false) (val-false))]
)

;; Default environment
(define default-environment (environment-with-values val-environment-empty [
    (negate (val-primitive-integer-unary-operation -))

    (+ (val-primitive-integer-binary-operation +))
    (- (val-primitive-integer-binary-operation -))
    (* (val-primitive-integer-binary-operation *))
    (/ (val-primitive-integer-binary-operation /))

    (= (val-primitive-integer-comparison =))
    (< (val-primitive-integer-comparison <))
    (<= (val-primitive-integer-comparison <=))
    (> (val-primitive-integer-comparison >))
    (>= (val-primitive-integer-comparison >=))
]))

;; DagContext :: Dict(Symbol, Dict(Any, Any))
;; Mutable store used for graph transform algorithm. Keys are compared by identity.
(define dag-context make-hash)
(struct dag-pending-token ())
(struct dag-pending-cylic-token (muarg is-cyclic))

;; dag-get-memoization-table :: DagContext -> Dict(Any, Any)
(define (dag-get-memoization-table context function-name)
  (if (hash-has-key? context function-name)
    (hash-ref context function-name)
    (let [(function-table (make-hasheq))]
      (hash-set! context function-name function-table)
      function-table
    )))

;; dag-get-memoization-equal-table :: DagContext -> Dict(Any, Any)
(define (dag-get-memoization-equal-table context function-name)
  (if (hash-has-key? context function-name)
    (hash-ref context function-name)
    (let [(function-table (make-hash))]
      (hash-set! context function-name function-table)
      function-table
    )))

;; dag-memoize :: DagContext, (Unit -> Any)
;; Common scaffolding required for non-cyclic dag algorithms.
(define (dag-memoize context function-name value transform)
  (define memoization-table (dag-get-memoization-table context function-name))
  (if (hash-has-key? memoization-table value)
    (match (hash-ref memoization-table value)
      [(dag-pending-token) (error "Cyclic expansion of " value)]
      [memoized-result memoized-result])
    (begin
      (hash-set! memoization-table value (dag-pending-token))
      (let [(transform-result (transform))]
        (hash-set! memoization-table value transform-result)
        transform-result))))

;; dag-cyclic-memoize :: DagContext, (Unit -> Any)
;; Common scaffolding required for cyclic-reducing dag rewriting algorithms.
;; This seems to be related to transfinite induction/recursion, and self-referencing systems.
;; Needed for the reduction of ((lambda (x) (x x x)) (lambda (x) (x x x))).
(define (dag-cyclic-memoize context function-name value muarg-function mu-function transform)
  (define memoization-table (dag-get-memoization-table context function-name))
  (if (hash-has-key? memoization-table value)
    (match (hash-ref memoization-table value)
      [(dag-pending-cylic-token muarg is-cyclic-box) (begin
        (set-box! is-cyclic-box #t)
        muarg)]
      [memoized-result memoized-result])
    (begin (let [(muarg (muarg-function)) (is-cyclic-box (box #f))]
      (hash-set! memoization-table value (dag-pending-cylic-token muarg is-cyclic-box))
      (let [(transform-result (transform))]
        (let [(result (if (unbox is-cyclic-box)
            (mu-function muarg transform-result)
            transform-result
          ))]
          (begin
            (hash-set! memoization-table value result)
            result
          )))))))

(struct dag-unif-val (value) #:transparent)

;; can-unify :: DagContext, Value -> Value
(define (can-unify value)
  (match value
    [(val-argument) #f]
    [(val-muargument) #f]
    [(val-epsargument) #f]
    [_ #t]))

;; unify :: DagContext, Value -> Value
(define (unify context value)
  (define (do-unify)
    (define unification-value (dag-unif-val value))
    (define unification-table (dag-get-memoization-equal-table context 'unification))
    (if (hash-has-key? unification-table unification-value)
      (hash-ref unification-table unification-value)
      (begin
        (hash-set! unification-table unification-value value)
        (let ([with-unified-children (val-recurse-children value (lambda (child) (unify context child)))])
          (hash-set! unification-table unification-value with-unified-children)
          value)
      )))

  (if (can-unify value)
    (do-unify)
    value))

;; comp-nullary-eps :: Value -> SyntaxValue
(define (comp-nullary-eps val)
  (val-eps (val-epsargument) val))

;; comp :: Syntax -> Value
(define (comp syntax)
  (match syntax
    [(stx-integer value) (comp-nullary-eps (val-integer value))]
    [(stx-true) (comp-nullary-eps (val-true))]
    [(stx-false) (comp-nullary-eps (val-false))]
    [(stx-unit) (comp-nullary-eps (val-unit))]

    [(stx-identifier name)
      (define context (val-epsargument))
      (val-eps context (val-dict-lookup context (val-symbol name)))]

    [(stx-if condition true-expression false-expression)
      (define context (val-epsargument))
      (define condition-expansion (val-expand (comp condition) context))
      (define true-expansion (val-expand (comp true-expression) context))
      (define false-expansion (val-expand (comp false-expression) context))
      (val-eps context (val-if condition-expansion true-expansion false-expansion))]

    [(stx-application functional argument)
      (define context (val-epsargument))
      (define functional-value (val-expand (comp functional) context))
      (define argument-value (val-expand (comp argument) context))
      (val-eps context (val-apply functional-value argument-value))]
    [(stx-expand macro argument)
      (define context (val-epsargument))
      (define macro-value (val-expand (comp macro) context))
      (define argument-value (val-expand (comp argument) context))
      (val-eps context (val-apply macro-value argument-value))]
    #|[(stx-epsilon argument body)
      (define context (val-epsargument))
      (define argument-value (val-epsargument))
      (define closure-environment (val-dict environment argument argument-value))
      (define closure-body (comp closure-environment body))
      (val-eps context (val-eps argument-value closure-body))]
    |#[(stx-lambda argument body)
      (define context (val-epsargument))
      (define argument-value (val-argument))
      (define closure-environment (val-dict context argument argument-value))
      (define closure-body (val-expand (comp body) closure-environment))
      (val-eps context (val-lambda argument-value closure-body))]
    [(stx-mu argument body)
      (define context (val-muargument))
      (define argument-value (val-argument))
      (define closure-environment (val-dict context argument argument-value))
      (define closure-body (val-expand (comp body) closure-environment))
      (val-eps context (val-mu argument-value closure-body))]
    [(stx-let name value body)
      (define context (val-epsargument))
      (define child-env (val-dict context name (val-expand (comp value) context)))
      (val-eps context (val-expand (comp body) child-env))]

    ;; Products
    [(stx-pair first second)
      (define context (val-epsargument))
      (define first-value (val-expand (comp first) context))
      (define second-value (val-expand (comp second) context))
      (val-eps context (val-pair first-value second-value))]
    [(stx-first pair)
      (define context (val-epsargument))
      (val-eps context (val-first (val-expand (comp pair) context)))]
    [(stx-second pair)
      (define context (val-epsargument))
      (val-eps context (val-second (val-expand (comp pair) context)))]

    ;; Sums
    [(stx-inject-left expression)
      (define context (val-epsargument))
      (val-eps (val-inject-left (val-expand (comp expression) context)))]
    [(stx-inject-right expression)
      (define context (val-epsargument))
      (val-eps (val-inject-right (val-expand (comp expression) context)))]
    [(stx-case expression left-case right-case)
      (define context (val-epsargument))
      (define expression-value (val-expand (comp expression) context))
      (define left-case-value (val-expand (comp left-case) context))
      (define right-case-value (val-expand (comp right-case) context))
      (val-eps (val-case expression-value left-case-value right-case-value))]
  ))

;; comp-sexpr :: SExpr -> Value
(define (comp-sexpr sexpr)
  (comp (parse-sexpr sexpr)))

;; is-constant-val :: DagContext, Value -> Boolean
(define (is-constant-val context value)
  (dag-memoize context 'is-constant-val value (lambda ()
      (match value
        [(val-integer _) #t]
        [(val-true) #t]
        [(val-false) #t]
        [(val-primitive _) #t]
        [(val-pair first second) (and (is-constant-val context first) (is-constant-val context second))]
        [(val-inject-left value) (is-constant-val context value)]
        [(val-inject-right value) (is-constant-val context value)]
        [_ #f]))))

(test-case
  "Is constant-expr"
  (define context (dag-context))
  [check-true (is-constant-val context (val-integer 42))]
)

;; val-recurse-children :: DagContext, Value -> Value
(define (val-recurse-children value rec)
  (match value
    [(val-apply functional argument) (val-apply (rec functional) (rec argument))]
    [(val-argument) value]
    [(val-dict parent symbol value) (val-dict (rec parent) (rec symbol) (rec value))]
    [(val-dict-lookup dict symbol) (val-dict-lookup (rec dict) (rec symbol))]
    [(val-epsargument) value]
    [(val-muargument) value]
    [(val-if condition true-block false-block) (val-if (rec condition) (rec true-block) (rec false-block))]
    [(val-integer _) value]
    [(val-true) value]
    [(val-false) value]
    [(val-symbol _) value]
    [(val-lambda argument body) (val-lambda (rec argument) (rec body))]
    [(val-mu argument body) (val-mu (rec argument) (rec body))]
    [(val-eps argument body) (val-eps (rec argument) (rec body))]
    [(val-expand macro argument) (val-expand (rec macro) (rec argument))]
    [(val-pair first second) (val-pair (rec first) (rec second))]
    [(val-first pair) (val-first (rec pair))]
    [(val-second pair) (val-second (rec pair))]
    [(val-primitive _) value]
    [(val-inject-left value) (val-inject-left (rec value))]
    [(val-inject-right value) (val-inject-right (rec value))]
    [(val-case value left-case right-case) (val-case (rec value) (rec left-case) (rec right-case))]))

;; dict-perform-lookup :: DagContext, Value -> Value
(define (dict-perform-lookup context lookup)
  (dag-memoize context 'dict-perform-lookup lookup (lambda ()
    (match lookup
      [(val-dict-lookup (val-dict parent key value) key) value]
      [(val-dict-lookup (val-dict parent _ _) key) (dict-perform-lookup context (val-dict-lookup parent key))]
      [_ #f]
    ))))

;; reduction-rule :: DagContext, Value -> Value
(define (reduction-rule context value)
  (match value
    [(val-apply (val-primitive primitive) argument) #:when (is-constant-val context argument)
      (primitive argument)] ;; Evaluate primitive with constants.
    ;[(val-dict-lookup (val-epsargument) _) value]
    ;[(val-dict-lookup (val-dict parent key dict-value) key) dict-value]
    ;[(val-dict-lookup (val-dict parent _ _) key) (val-dict-lookup parent key)]

    [(val-dict-lookup dict key) #:when (dict-perform-lookup context value)
      (dict-perform-lookup context value)] ;; Reduce known keys
    [(val-apply (val-lambda argument-definition body) argument-value)
      (substitute context argument-definition argument-value body)]
    [(val-expand (val-eps argument-definition body) expand-context)
      (substitute context argument-definition expand-context body)]
    [(val-expand non-syntax-value expand-context) non-syntax-value]
    [(val-mu argument body)
      (if (uses-var? context body argument)
        value
        body)]
    [(val-if (val-true) true-block _) true-block]
    [(val-if (val-false) _ false-block) false-block]
    [(val-if expr result result) result]
    [(val-first (val-pair first _)) first]
    [(val-second (val-pair _ second)) second]
    [(val-case (val-inject-left value) left-case _) (val-apply left-case value)]
    [(val-case (val-inject-right value) _ right-case) (val-apply right-case value)]
    [value value]))

;; reduce-once :: DagContext, Value -> Value
(define (reduce-once context value)
  (dag-cyclic-memoize context 'reduce-once value val-muargument val-mu (lambda ()
      (define with-reduced-child (unify context (val-recurse-children value (lambda (child) (reduce-once context child)))))
      (unify context (reduction-rule context with-reduced-child)))))

;; find-used-var-set :: DagContext Expression -> Set(Value)
(define (find-used-var-set context expression)
  (dag-memoize context 'find-used-var-set expression (lambda ()
    (match expression
      [(val-apply functional argument) (set-union (find-used-var-set context functional) (find-used-var-set context argument))]
      [(val-expand macro argument) (set-union (find-used-var-set context macro) (find-used-var-set context argument))]
      [(val-argument) (seteq expression)]
      [(val-dict parent symbol value) (set-union (find-used-var-set context parent) (find-used-var-set context symbol) (find-used-var-set context value))]
      [(val-dict-lookup dict symbol) (set-union (find-used-var-set context dict) (find-used-var-set context symbol))]
      [(val-if condition true-block false-block) (set-union (find-used-var-set context condition) (find-used-var-set context true-block) (find-used-var-set context false-block))]
      [(val-integer _) (seteq)]
      [(val-true) (seteq)]
      [(val-false) (seteq)]
      [(val-symbol _) (seteq)]
      [(val-lambda argument body) (set-union (find-used-var-set context argument) (find-used-var-set context body))]
      [(val-eps argument body) (set-union (find-used-var-set context argument) (find-used-var-set context body))]
      [(val-epsargument) (seteq expression)]
      [(val-mu argument body) (set-union (find-used-var-set context argument) (find-used-var-set context body))]
      [(val-muargument) (seteq expression)]
      [(val-pair first second) (set-union (find-used-var-set context first) (find-used-var-set context second))]
      [(val-first pair) (find-used-var-set pair)]
      [(val-second pair) (find-used-var-set pair)]
      [(val-primitive _) (seteq)]
      [(val-inject-left value) (find-used-var-set value)]
      [(val-inject-right value) (find-used-var-set value)]
      [(val-case value left-case right-case) (set-union (find-used-var-set value) (find-used-var-set left-case) (find-used-var-set right-case))]))
    ))

;; uses-var? :: DagContext Expression Variable -> Set(Value)
(define (uses-var? context expression var)
  (set-member? (find-used-var-set context expression) var))

(test-case
  "Find used variable set"
  (check-true (set-empty? (find-used-var-set (dag-context) (val-integer 42))))

  (define arg_x (val-argument))
  (define arg_y (val-argument))
  (check-true (set-member? (find-used-var-set (dag-context) arg_x) arg_x))
  (check-false (set-member? (find-used-var-set (dag-context) arg_x) arg_y))

  (check-true (set-member? (find-used-var-set (dag-context) (val-apply arg_x arg_x)) arg_x))
  (check-true (set-member? (find-used-var-set (dag-context) (val-apply arg_x arg_y)) arg_x))
  (check-true (set-member? (find-used-var-set (dag-context) (val-apply arg_x arg_y)) arg_y))
)

;; substitute :: Original, Substitution, Expression -> Value
(define (substitute context original substitution expression)
  (if (eq? original expression)
    substitution
    (if (set-member? (find-used-var-set context expression) original)
      (dag-memoize context 'substitute expression (lambda ()
        (val-recurse-children expression (lambda (child) (substitute context original substitution child)))))
      expression)))

;; reduce-once-sexpr :: Environment, SExpr -> Value
(define (reduce-once-sexpr environment sexpr)
  (reduce-once (dag-context) (val-expand (comp-sexpr sexpr) environment)))

;; reduce-once-def-sexpr :: SExpr -> Value
(define (reduce-once-def-sexpr sexpr)
  (reduce-once-sexpr default-environment sexpr))

;; reduce :: DagContext, Value -> Value
;; Reduce until achieving a fixpoint.
(define (reduce context value)
  (dag-cyclic-memoize context 'reduce value val-muargument val-mu (lambda ()
      (define with-reduced-child (unify context (val-recurse-children value (lambda (child) (reduce context child)))))
      (define reduced-once (unify context (reduction-rule context with-reduced-child)))
      (if (eq? with-reduced-child reduced-once)
        reduced-once
        (reduce context reduced-once)))))

;; reduce-sexpr :: Environment, SExpr -> Value
(define (reduce-sexpr environment sexpr)
  (reduce (dag-context) (val-expand (comp-sexpr sexpr) environment)))

;; reduce-def-sexpr :: SExpr -> Value
(define (reduce-def-sexpr sexpr)
  (reduce-sexpr default-environment sexpr))

(graph-to-svg-file (comp-sexpr '(+ 1 2)) "plus-syntax.svg")
(graph-to-svg-file (reduce-once-def-sexpr '(+ 1 2)) "plus-reduced.svg")

(test-case
  "Reduction test case"
  [check-equal?
    (reduce-once-def-sexpr 1)
    (val-integer 1)]
  [check-equal?
    (reduce-once-def-sexpr #t)
    (val-true)]
  [check-equal?
    (reduce-once-def-sexpr #f)
    (val-false)]
  [check-equal?
    (reduce-once-def-sexpr '(not #t))
    (val-false)]
  [check-equal?
    (reduce-once-def-sexpr '(not #f))
    (val-true)]
  [check-equal?
    (reduce-once-def-sexpr '(if 4 1 1))
    (val-integer 1)]
  [check-equal?
    (reduce-once-def-sexpr '(if #t 1 2))
    (val-integer 1)]
  [check-equal?
    (reduce-once-def-sexpr '(if #f 1 2))
    (val-integer 2)]
  [check-equal?
    (reduce-def-sexpr '(let [(x 42)] x))
    (val-integer 42)]
  [check-equal?
    (reduce-def-sexpr '(let [(x 42)] 5))
    (val-integer 5)]
  (define arg_x (val-argument))
  [check-equal?
    (reduce-def-sexpr '(lambda (x) x))
    (val-lambda arg_x arg_x)]
  [check-equal?
    (reduce-def-sexpr '((lambda (x) x) 42))
    (val-integer 42)]
  [check-equal?
    (reduce-def-sexpr '((lambda (x y) y) 2 5))
    (val-integer 5)]

  [check-equal?
    (reduce-def-sexpr '(mu (x) 5))
    (val-integer 5)]

  [check-equal?
    (reduce-def-sexpr '(+ 2 3))
    (val-integer 5)]

  ;; Products
  [check-equal?
    (reduce-once-def-sexpr '(pair 1 2))
    (val-pair (val-integer 1) (val-integer 2))]
  [check-equal?
    (reduce-once-def-sexpr '(first (pair 1 2)))
    (val-integer 1)]
  [check-equal?
    (reduce-once-def-sexpr '(second (pair 1 2)))
    (val-integer 2)]

  ;; Sums
  [check-equal?
    (reduce-once-def-sexpr '(inject-left 1))
    (val-inject-left (val-integer 1))]
  [check-equal?
    (reduce-once-def-sexpr '(inject-right 1))
    (val-inject-right (val-integer 1))]
  [check-equal?
    (reduce-once-def-sexpr '(case (inject-left 1) (lambda (x) x) (lambda (x) (+ x 1))))
    (val-apply (val-lambda arg_x arg_x) (val-integer 1))]
  [check-equal?
    (reduce-once (dag-context) (reduce-once-def-sexpr '(case (inject-left 1) (lambda (x) x) (lambda (x) (+ x 1)))))
    (val-integer 1)]

  [check-equal?
    (reduce-once (dag-context) (reduce-once (dag-context) (reduce-once-def-sexpr '(case (inject-right 1) (lambda (x) x) (lambda (x) (+ x 1))))))
    (val-integer 2)]
)

;(define ctx '(epsilon (gamma) gamma))
;(graph-to-svg-file (parse-sexpr ctx) "ctx-syntax.svg")
;(graph-to-svg-file (comp-sexpr ctx) "ctx-comp.svg")
;(graph-to-svg-file (reduce-def-sexpr ctx) "ctx-reduced.svg")
