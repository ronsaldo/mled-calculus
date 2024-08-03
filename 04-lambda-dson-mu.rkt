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
(struct stx-let (name value body) #:transparent)
(struct stx-if (condition true-expression false-expression) #:transparent)
(struct stx-identifier (symbol) #:transparent)
(struct stx-pair (first second) #:transparent)
(struct stx-first (pair) #:transparent)
(struct stx-second (second) #:transparent)
(struct stx-inject-left (expression) #:transparent)
(struct stx-inject-right (expression) #:transparent)
(struct stx-case (expression left right) #:transparent)

;; parse-sexpr :: String -> Syntax
;; Syntax Grammar:
;; expr ::= <integer>
;;        | <boolean>
;;        | <identifier>
;;        | ()
;;        | (if <expr> <expr> <expr>)
;;        | (lambda (<identifier>*) <expr>)
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
;;        | (<expr> <expr>)
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
      ([list 'lambda arguments body] (foldr stx-lambda (parse-sexpr body) arguments))
      ([list 'let nameValues body]
        (foldr (lambda (nameValuePair body)
          (stx-let (first nameValuePair) (parse-sexpr (second nameValuePair)) body)
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
  (check-equal? (parse-sexpr '()) (stx-unit))
  (check-equal? (parse-sexpr '(if #t 1 2)) (stx-if (stx-true) (stx-integer 1) (stx-integer 2)))
  (check-equal? (parse-sexpr '(lambda (x) x)) (stx-lambda 'x (stx-identifier 'x)))
  (check-equal? (parse-sexpr '(lambda (x y) x)) (stx-lambda 'x (stx-lambda 'y (stx-identifier 'x))))
  (check-equal? (parse-sexpr '(let [(x 42)] x)) (stx-let 'x (stx-integer 42) (stx-identifier 'x)))
  (check-equal? (parse-sexpr '(let [(x 42) (y 45)] y)) (stx-let 'x (stx-integer 42) (stx-let 'y (stx-integer 45) (stx-identifier 'y))))
  (check-equal? (parse-sexpr '(pair 1 2)) (stx-pair (stx-integer 1) (stx-integer 2)))
  (check-equal? (parse-sexpr '(first x)) (stx-first (stx-identifier 'x)))
  (check-equal? (parse-sexpr '(second x)) (stx-second (stx-identifier 'x)))
  (check-equal? (parse-sexpr '(inject-left 42)) (stx-inject-left (stx-integer '42)))
  (check-equal? (parse-sexpr '(inject-right 42)) (stx-inject-right (stx-integer '42)))
  (check-equal? (parse-sexpr '(case x y z)) (stx-case (stx-identifier 'x) (stx-identifier 'y) (stx-identifier 'z)) )
)

;; Environment :: Empty | Environment (Symbol -> Value)
(struct environment-empty () #:transparent)
(struct environment-child (parent symbol value) #:transparent)

;; Compiler Value ::
;;                 | VApply(Value, Value)
;;                 | VArgument
;;                 | VIf(Value, Value, Value) 
;;                 | VInteger | VTrue | VFalse
;;                 | VLambda(VArgument, Value)
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
(struct val-apply (functional argument) #:transparent)
(struct val-argument () #:transparent)
(struct val-if (condition true-block false-block) #:transparent)
(struct val-integer (value) #:transparent)
(struct val-true () #:transparent)
(struct val-false () #:transparent)
(struct val-lambda (argument body) #:transparent)
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

;; val-primitive-boolean-unary-operation :: (Boolean -> Boolean) -> val-primitive
(define (val-primitive-boolean-unary-operation operation)
  (val-primitive (lambda (argument)
    (define argument-value (val-unwrap-boolean argument))
    (val-wrap-boolean (operation argument-value)))))

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
    ((val-primitive-implementation (val-primitive-boolean-unary-operation not)) (val-true))
    (val-false)]
  [check-equal?
    ((val-primitive-implementation (val-primitive-boolean-unary-operation not)) (val-false))
    (val-true)]

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
      (environment-with-values-expansion (environment-child environment symbol value) rest)]))

;; environment-with-values :: Environment, List(Symbol, Value) -> Environment
(define-syntax environment-with-values
  (syntax-rules ()
    [(_ base-environment ([symbol value]...))
      (environment-with-values-expansion base-environment (list (list (quote symbol) value) ...))
    ]))

(test-case
  "environment-with-values"
  [check-equal?
    (environment-with-values (environment-empty) [])
    (environment-empty)]
  [check-equal?
    (environment-with-values (environment-empty) [(true (val-true))])
    (environment-child (environment-empty) 'true (val-true))]
  [check-equal?
    (environment-with-values (environment-empty) [(true (val-true)) (false (val-false))])
    (environment-child (environment-child (environment-empty) 'true (val-true)) 'false (val-false))]
)

;; lookup-valid-symbol :: Environment, Symbol -> Value. Error when not found.
(define (lookup-valid-symbol environment symbol)
  (match environment
    [(environment-empty) (error "Unbound symbol during compilation " symbol)]
    [(environment-child parent env-symbol value)
      (if (symbol=? env-symbol symbol)
        value
        (lookup-valid-symbol parent symbol))]
  ))

(test-case
  "lookup-valid-symbol"
  (check-equal? (lookup-valid-symbol (environment-child (environment-empty) 'x (val-integer 2)) 'x) (val-integer 2))
)

;; Default environment
(define default-environment (environment-with-values (environment-empty) [
    (not (val-primitive-boolean-unary-operation not))

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

;; comp :: Environment, Syntax -> Value
(define (comp environment syntax)
  (match syntax
    [(stx-integer value) (val-integer value)]
    [(stx-true) (val-true)]
    [(stx-false) (val-false)]
    [(stx-unit) (val-unit)]
    [(stx-identifier name) (lookup-valid-symbol environment name)]
    [(stx-if condition true-expression false-expression)
      (define condition-value (comp environment condition))
      (define true-value (comp environment true-expression))
      (define false-value (comp environment false-expression))
      (val-if condition-value true-value false-value)]
    [(stx-application functional argument)
      (define functional-value (comp environment functional))
      (define argument-value (comp environment argument))
      (val-apply functional-value argument-value)]
    [(stx-lambda argument body)
      (define argument-value (val-argument))
      (define closure-environment (environment-child environment argument argument-value))
      (define closure-body (comp closure-environment body))
      (val-lambda argument-value closure-body)]
    [(stx-let name value body)
      (define child-env (environment-child environment name (comp environment value)))
      (comp child-env body)]

    ;; Products
    [(stx-pair first second)
      (define first-value (comp environment first))
      (define second-value (comp environment second))
      (val-pair first-value second-value)]
    [(stx-first pair) (val-first (comp environment pair))]
    [(stx-second pair) (val-second (comp environment pair))]

    ;; Sums
    [(stx-inject-left expression) (val-inject-left (comp environment expression))]
    [(stx-inject-right expression) (val-inject-right (comp environment expression))]
    [(stx-case expression left-case right-case)
      (define expression-value (comp environment expression))
      (define left-case-value (comp environment left-case))
      (define right-case-value (comp environment right-case))
      (val-case expression-value left-case-value right-case-value)]
  ))

;; comp-sexpr :: Environment, SExpr -> Value
(define (comp-sexpr environment sexpr)
  (comp environment (parse-sexpr sexpr)))

;; comp-def-sexpr :: SExpr -> Value
(define (comp-def-sexpr sexpr)
  (comp-sexpr default-environment sexpr))

(test-case
  "Compiler test"
  [check-equal?
    (comp-def-sexpr 1)
    (val-integer 1)]
  [check-equal?
    (comp-def-sexpr #t)
    (val-true)]
  [check-equal?
    (comp-def-sexpr #f)
    (val-false)]
  [check-equal?
    (comp-def-sexpr '(if #t 1 2))
    (val-if (val-true) (val-integer 1) (val-integer 2))]
  [check-equal?
    (comp-def-sexpr '(if #f 1 2))
    (val-if (val-false) (val-integer 1) (val-integer 2))]
  [check-equal?
    (comp-def-sexpr '(let [(x 42)] x))
    (val-integer 42)]
  [check-equal?
    (comp-def-sexpr '(let [(x 42)] 5))
    (val-integer 5)]
  (define arg_x (val-argument))
  (define arg_y (val-argument))
  [check-equal?
    (comp-def-sexpr '(lambda (x) x))
    (val-lambda arg_x arg_x)]
  [check-equal?
    (comp-def-sexpr '((lambda (x) x) 42))
    (val-apply (val-lambda arg_x arg_x) (val-integer 42))]
  [check-equal?
    (comp-def-sexpr '((lambda (x y) y) 2 5))
    (val-apply (val-apply (val-lambda arg_x (val-lambda arg_y arg_y)) (val-integer 2)) (val-integer 5))]

  (define prim-+ (lookup-valid-symbol default-environment '+))
  [check-equal?
    (comp-def-sexpr '(+ 2 3))
    (val-apply (val-apply prim-+ (val-integer 2)) (val-integer 3))]

  ;; Products
  [check-equal?
    (comp-def-sexpr '(pair 1 2))
    (val-pair (val-integer 1) (val-integer 2))]
  [check-equal?
    (comp-def-sexpr '(first (pair 1 2)))
    (val-first (val-pair (val-integer 1) (val-integer 2)))]
  [check-equal?
    (comp-def-sexpr '(second (pair 1 2)))
    (val-second (val-pair (val-integer 1) (val-integer 2)))]

  ;; Sums
  [check-equal?
    (comp-def-sexpr '(inject-left 1))
    (val-inject-left (val-integer 1))]
  [check-equal?
    (comp-def-sexpr '(inject-right 1))
    (val-inject-right (val-integer 1))]
  [check-equal?
    (comp-def-sexpr '(case (inject-left 1) (lambda (x) x) (lambda (x) (+ x 1))))
    (val-case (val-inject-left (val-integer 1))
      [val-lambda arg_x arg_x]
      (val-lambda arg_x
        (val-apply (val-apply prim-+ arg_x) (val-integer 1)))
    )]
  [check-equal?
    (comp-def-sexpr '(case (inject-right 1) (lambda (x) x) (lambda (x) (+ x 1))))
    (val-case (val-inject-right (val-integer 1))
      [val-lambda arg_x arg_x]
      (val-lambda arg_x
        (val-apply (val-apply prim-+ arg_x) (val-integer 1)))
    )]
)

;; is-constant-val :: DagContext, Value -> Boolean
(define (is-constant-val context value)
  (dag-memoize context 'is-constant-val value (lambda ()
      (match value
        [(val-integer _) #t]
        [(val-true) #t]
        [(val-false) #f]
        [(val-primitive _) #t]
        [(val-pair first second) (and (is-constant-val context first) (is-constant-val context second))]
        [(val-inject-left value) (is-constant-val context value)]
        [(val-inject-right value) (is-constant-val context value)]
        [_ #f]))))

;; is-final-constant-val :: Value -> Boolean
(define (is-final-constant-val value)
  (match value
    [(val-lambda _ _) value]
    [(val-mu _ _) value]
    [_ value]))

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
    [(val-muargument) value]
    [(val-if condition true-block false-block) (val-if (rec condition) (rec true-block) (rec false-block))]
    [(val-integer _) value]
    [(val-true) value]
    [(val-false) value]
    [(val-lambda argument body) (val-lambda (rec argument) (rec body))]
    [(val-mu argument body) (val-mu (rec argument) (rec body))]
    [(val-pair first second) (val-pair (rec first) (rec second))]
    [(val-first pair) (val-first (rec pair))]
    [(val-second pair) (val-second (rec pair))]
    [(val-primitive _) value]
    [(val-inject-left value) (val-inject-left (rec value))]
    [(val-inject-right value) (val-inject-right (rec value))]
    [(val-case value left-case right-case) (val-case (rec value) (rec left-case) (rec right-case))]))

;; reduction-rule :: DagContext, Value -> Value
(define (reduction-rule context value)
  (match value
    [(val-apply (val-primitive primitive) argument) #:when (is-constant-val context argument)
      (primitive argument)] ;; Evaluate primitive with constants.
    [(val-apply (val-lambda argument-definition body) argument-value)
      (substitute (dag-context) argument-definition argument-value body)]
    [(val-if (val-true) true-block _) true-block]
    [(val-if (val-false) _ false-block) false-block]
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

;; substitute :: Original, Substitution, Expression -> Value
(define (substitute context original substitution expression)
  (if (eq? original expression)
    substitution
    (dag-memoize context 'substitute expression (lambda ()
      (val-recurse-children expression (lambda (child) (substitute context original substitution child)))))))

;; reduce-once-sexpr :: Environment, SExpr -> Value
(define (reduce-once-sexpr environment sexpr)
  (reduce-once (dag-context) (comp environment (parse-sexpr sexpr))))

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
  (reduce (dag-context) (comp-sexpr environment sexpr)))

;; reduce-def-sexpr :: SExpr -> Value
(define (reduce-def-sexpr sexpr)
  (reduce-sexpr default-environment sexpr))

(test-case
  "Reduction once test case"
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
    (reduce-once-def-sexpr '(if #t 1 2))
    (val-integer 1)]
  [check-equal?
    (reduce-once-def-sexpr '(if #f 1 2))
    (val-integer 2)]
  [check-equal?
    (reduce-once-def-sexpr '(let [(x 42)] x))
    (val-integer 42)]
  [check-equal?
    (reduce-once-def-sexpr '(let [(x 42)] 5))
    (val-integer 5)]
  (define arg_x (val-argument))
  [check-equal?
    (comp-def-sexpr '(lambda (x) x))
    (val-lambda arg_x arg_x)]
  [check-equal?
    (reduce-once-def-sexpr '((lambda (x) x) 42))
    (val-integer 42)]
  [check-equal?
    (reduce-once-def-sexpr '((lambda (x y) y) 2 5))
    (val-integer 5)]
  [check-equal?
    (reduce-once-def-sexpr '(+ 2 3))
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
 
  (define prim-+ (lookup-valid-symbol default-environment '+))
  [check-equal?
    (reduce-once-def-sexpr '(case (inject-right 1) (lambda (x) x) (lambda (x) (+ x 1))))
    (val-apply (val-lambda arg_x
        (val-apply (val-apply prim-+ arg_x) (val-integer 1))) (val-integer 1))]
 
  [check-equal?
    (reduce-once (dag-context) (reduce-once-def-sexpr '(case (inject-right 1) (lambda (x) x) (lambda (x) (+ x 1)))))
    (val-apply (val-apply prim-+ (val-integer 1)) (val-integer 1))]

  [check-equal?
    (reduce-once (dag-context) (reduce-once (dag-context) (reduce-once-def-sexpr '(case (inject-right 1) (lambda (x) x) (lambda (x) (+ x 1))))))
    (val-integer 2)]
)

;; interp-sexpr :: Environment, SExpr -> Value
(define (interp-sexpr environment sexpr)
  (define reduced (reduce-sexpr environment sexpr))
  (if (is-final-constant-val reduced)
    reduced
    ("Error. Failed to reduce to final constant redex")))

;; interp-def-sexpr :: SExpr -> Value
(define (interp-def-sexpr sexpr)
  (interp-sexpr default-environment sexpr))

(define omega '((lambda (x) (x x)) (lambda (x) (x x))))
(graph-to-svg-file (parse-sexpr omega) "omega-syntax.svg")
(graph-to-svg-file (comp-def-sexpr omega) "omega-comp.svg")
(graph-to-svg-file (reduce-once-def-sexpr omega) "omega-reduced-once.svg")
(graph-to-svg-file (reduce-once (dag-context) (reduce-once-def-sexpr omega)) "omega-reduced-once2.svg")
(graph-to-svg-file (reduce-def-sexpr omega) "omega-reduced")

(define omega_omega '(((lambda (x) (x x)) (lambda (x) (x x))) ((lambda (x) (x x)) (lambda (x) (x x)))) )
(graph-to-svg-file (parse-sexpr omega_omega) "omega-omega-syntax.svg")
(graph-to-svg-file (comp-def-sexpr omega_omega) "omega-omega-comp.svg")
(graph-to-svg-file (reduce-once-def-sexpr omega_omega) "omega-omega-reduced-once.svg")
(graph-to-svg-file (reduce-once (dag-context) (reduce-once-def-sexpr omega_omega)) "omega-omega-reduced-once2.svg")
(graph-to-svg-file (reduce-def-sexpr omega_omega) "omega-omega-reduced")

(define omega3 '((lambda (x) (x x x)) (lambda (x) (x x x))))
(graph-to-svg-file (parse-sexpr omega3) "omega3-syntax.svg")
(graph-to-svg-file (comp-def-sexpr omega3) "omega3-comp.svg")
(graph-to-svg-file (reduce-once-def-sexpr omega3) "omega3-reduced-once.svg")
(graph-to-svg-file (reduce-once (dag-context) (reduce-once-def-sexpr omega3)) "omega3-reduced-once2.svg")
(graph-to-svg-file (reduce-once (dag-context) (reduce-once (dag-context) (reduce-once-def-sexpr omega3))) "omega3-reduced-once3.svg")
(graph-to-svg-file (reduce-once (dag-context) (reduce-once (dag-context) (reduce-once (dag-context) (reduce-once-def-sexpr omega3)))) "omega3-reduced-once4.svg")
(graph-to-svg-file (reduce-def-sexpr omega3) "omega3-reduced") ; This one crashes. Duplicated recursive state.

(define test-code '((lambda (x y) x) 42))
;;(define test-code '(+ 1 2))
(graph-to-svg-file (parse-sexpr test-code) "syntax.svg")
(graph-to-svg-file (comp-def-sexpr test-code) "comp.svg")
(graph-to-svg-file (reduce-once-def-sexpr test-code) "reduced-once.svg")
;;(graph-to-svg-file (reduce-def-sexpr test-code) "reduced.svg")

(define id_arg (val-argument))
(define id_fun (val-lambda id_arg id_arg))
(define cyclic_id (val-apply id_fun id_fun))

(graph-to-svg-file cyclic_id "cyclic-id.svg")
(graph-to-svg-file (reduce-once (dag-context) cyclic_id) "cyclic-id-reduced-once.svg")
(graph-to-svg-file (reduce (dag-context) cyclic_id) "cyclic-id-reduced.svg")
