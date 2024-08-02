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

;; Interpreter Value :: VInteger | VTrue | VFalse
;;                     | VClosure(Environment, Symbol, Syntax) 
;;                     | VPrimitive(Value -> Value)
;;                     | VUnit | VPair(Value, Value)
;;                     | VInjectLeft(Value) | VInjectRight(Value)
(struct val-integer (value) #:transparent)
(struct val-true () #:transparent)
(struct val-false () #:transparent)
(struct val-closure (environment argument body) #:transparent)
(struct val-pair (first second) #:transparent)
(struct val-primitive (implementation) #:transparent)
(struct val-unit () #:transparent)
(struct val-inject-left (value) #:transparent)
(struct val-inject-right (value) #:transparent)

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
    [(environment-empty) (error "Unbound symbol during interpretation: " symbol)]
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

;; interp :: Environment, Syntax -> Value
(define (interp environment syntax)
  (match syntax
    [(stx-integer value) (val-integer value)]
    [(stx-true) (val-true)]
    [(stx-false) (val-false)]
    [(stx-unit) (val-unit)]
    [(stx-identifier name) (lookup-valid-symbol environment name)]
    [(stx-if condition true-expression false-expression)
      (define condition-value (interp environment condition))
      (match condition-value
        [(val-true) (interp environment true-expression)]
        [(val-false) (interp environment false-expression)])]
    [(stx-application functional argument)
      (define functional-value (interp environment functional))
      (define argument-value (interp environment argument))
      (interp-apply-value-with-value environment functional-value argument-value)]
    [(stx-lambda argument body)
      (val-closure environment argument body)]
    [(stx-let name value body)
      (define child-env (environment-child environment name (interp environment value)))
      (interp child-env body)]

    ;; Products
    [(stx-pair first second)
      (define first-value (interp environment first))
      (define second-value (interp environment second))
      (val-pair first-value second-value)]
    [(stx-first pair)
      (match (interp environment pair)
        [(val-pair first second) first])]
    [(stx-second pair)
      (match (interp environment pair)
        [(val-pair first second) second])]

    ;; Sums
    [(stx-inject-left expression) (val-inject-left (interp environment expression))]
    [(stx-inject-right expression) (val-inject-right (interp environment expression))]
    [(stx-case expression left-case right-case)
      (match (interp environment expression)
        [(val-inject-left injected-value) (interp-apply-with-value environment left-case injected-value)]
        [(val-inject-right injected-value) (interp-apply-with-value environment right-case injected-value)])]
  ))

;; interp-apply-value-with-value :: Environment, Value, Value -> Value
(define (interp-apply-value-with-value environment functional argument)
  (match functional
    [(val-closure closure-environment closure-argument closure-body)
      (define activation-environment (environment-child closure-environment closure-argument argument))
      (interp activation-environment closure-body)]
    [(val-primitive implementation) (implementation argument)]))

;; interp-apply-value-with-value :: Environment, Syntax, Value -> Value
(define (interp-apply-with-value environment functional-expr argument)
  (define functional-value (interp environment functional-expr))
  (interp-apply-value-with-value environment functional-value argument))

;; interp-sexpr :: Environment, SExpr -> Value
(define (interp-sexpr environment sexpr)
  (interp environment (parse-sexpr sexpr)))

;; interp-def-sexpr :: SExpr -> Value
(define (interp-def-sexpr sexpr)
  (interp-sexpr default-environment sexpr))

(test-case
  "Interpreter"
  [check-equal?
    (interp-def-sexpr 1)
    (val-integer 1)]
  [check-equal?
    (interp-def-sexpr #t)
    (val-true)]
  [check-equal?
    (interp-def-sexpr #f)
    (val-false)]
  [check-equal?
    (interp-def-sexpr '(if #t 1 2))
    (val-integer 1)]
  [check-equal?
    (interp-def-sexpr '(if #f 1 2))
    (val-integer 2)]
  [check-equal?
    (interp-def-sexpr '(let [(x 42)] x))
    (val-integer 42)]
  [check-equal?
    (interp-def-sexpr '(let [(x 42)] 5))
    (val-integer 5)]
  [check-equal?
    (interp-def-sexpr '(lambda (x) x))
    (val-closure default-environment 'x (stx-identifier 'x))]
  [check-equal?
    (interp-def-sexpr '((lambda (x) x) 42))
    (val-integer 42)]
  [check-equal?
    (interp-def-sexpr '((lambda (x y) y) 2 5))
    (val-integer 5)]
  [check-equal?
    (interp-def-sexpr '(+ 2 3))
    (val-integer 5)]

  ;; Products
  [check-equal?
    (interp-def-sexpr '(pair 1 2))
    (val-pair (val-integer 1) (val-integer 2))]
  [check-equal?
    (interp-def-sexpr '(first (pair 1 2)))
    (val-integer 1)]
  [check-equal?
    (interp-def-sexpr '(second (pair 1 2)))
    (val-integer 2)]

  ;; Sums
  [check-equal?
    (interp-def-sexpr '(inject-left 1))
    (val-inject-left (val-integer 1))]
  [check-equal?
    (interp-def-sexpr '(inject-right 1))
    (val-inject-right (val-integer 1))]
  [check-equal?
    (interp-def-sexpr '(case (inject-left 1) (lambda (x) x) (lambda (x) (+ x 1))))
    (val-integer 1)]
  [check-equal?
    (interp-def-sexpr '(case (inject-right 1) (lambda (x) x) (lambda (x) (+ x 1))))
    (val-integer 2)]
)

;; Omega
;;(println "Running omega. This should never end or stack overflow.")
;;(interp-def-sexpr '((lambda (x) (x x)) (lambda (x) (x x))))

;(save-to-svg-file (parse-sexpr '((lambda (x) x) 42)) "syntax.svg")
;(save-to-svg-file (interp-def-sexpr '(lambda (x) x)) "test.svg")
