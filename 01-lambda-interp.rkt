#lang racket

(require racket/match)
(require rackunit)

; AST Syntactic nodes.
(struct stx-integer (value) #:transparent)
(struct stx-true () #:transparent)
(struct stx-false () #:transparent)
(struct stx-application (functional argument) #:transparent)
(struct stx-lambda (argument body) #:transparent)
(struct stx-let (name value body) #:transparent)
(struct stx-identifier (symbol) #:transparent)

;; parse-sexpr :: String -> Syntax
(define (parse-sexpr sexpr)
  (cond
    [(integer? sexpr) (stx-integer sexpr)]
    [(boolean? sexpr) (if sexpr (stx-true) (stx-false))]
    [(symbol? sexpr) (stx-identifier sexpr)]
    [(list? sexpr) [match sexpr
      ([list 'lambda arguments body] (foldr stx-lambda (parse-sexpr body) arguments))
      ([list 'let nameValues body]
        (foldr (lambda (nameValuePair body)
          (stx-let (first nameValuePair) (parse-sexpr (second nameValuePair)) body)
        ) (parse-sexpr body) nameValues))
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

  (check-equal? (parse-sexpr '(lambda (x) x)) (stx-lambda 'x (stx-identifier 'x)))
  (check-equal? (parse-sexpr '(lambda (x y) x)) (stx-lambda 'x (stx-lambda 'y (stx-identifier 'x))))
  (check-equal? (parse-sexpr '(let [(x 42)] x)) (stx-let 'x (stx-integer 42) (stx-identifier 'x)))
  (check-equal? (parse-sexpr '(let [(x 42) (y 45)] y)) (stx-let 'x (stx-integer 42) (stx-let 'y (stx-integer 45) (stx-identifier 'y))))
)
;; Environment :: Empty | Environment (Symbol -> Value)
(struct environment-empty () #:transparent)
(struct environment-child (parent symbol value) #:transparent)

;; Interpreter Values :: Integer | True | False | Closure(Environment, Symbol, Syntax)
(struct val-integer (value) #:transparent)
(struct val-true () #:transparent)
(struct val-false () #:transparent)
(struct val-closure (environment argument body) #:transparent)
(struct val-primitive (name implementation) #:transparent)

;; lookup-valid-symbol :: Environment , Symbol -> Value. Error when not found.
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
(define default-environment (environment-empty))

;; interp :: Environment, Syntax -> Value
(define (interp environment syntax)
  (match syntax
    [(stx-integer value) (val-integer value)]
    [(stx-true) (val-true)]
    [(stx-false) (val-false)]
    [(stx-identifier name) (lookup-valid-symbol environment name)]
    [(stx-application functional argument)
      (match (interp environment functional)
        [(val-closure closure-environment closure-argument closure-body)
          (define activation-argument (interp environment argument))
          (define activation-environment (environment-child closure-environment closure-argument activation-argument))
          (interp activation-environment closure-body)]
      )
    ]
    [(stx-lambda argument body)
      (val-closure environment argument body)]
    [(stx-let name value body)
      (define child-env (environment-child environment name (interp environment value)))
      (interp child-env body)]
  ))

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
)
