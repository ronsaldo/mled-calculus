#lang racket

(require racket/dict)
(require racket/format)
(require racket/set)

(provide save-to-dot-file)
(provide graph-to-svg)
(provide save-to-svg-file)

;; dependencies-of-object :: Any -> List[Any]
(define (dependencies-of-object object)
    (cond
        [(struct? object) (filter struct? (rest (vector->list (struct->vector object))))]
        [else (list)]))

;; graph-dfs :: List[Any] -> (Any-> List[Any]) -> List[Any]
(define (graph-dfs roots)
    (define visited-set (mutable-seteq))
    (define sorted-nodes (box (list)))
    (define (visit node)
        (if (set-member? visited-set node)
            void
            (begin
                (set-add! visited-set node)
                (let [(children (dependencies-of-object node))]
                    (for-each visit children))
                (set-box! sorted-nodes (cons node (unbox sorted-nodes))))))
    (for-each visit roots)
    (reverse (unbox sorted-nodes)))

;; enumerate-graph-nodes :: List[Any] -> Dict[Any, Integer]
(define (enumerate-graph-nodes nodes)
    (define mapping (make-hasheq))
    (define index (box 0))
    (for-each (lambda (node)
        (hash-set! mapping node (unbox index))
        (set-box! index (+ (unbox index) 1)))
        nodes)
    mapping)

;; graph-pretty-label :: Any -> String
(define (graph-pretty-label node)
    (cond
        [(struct? node) (apply string-append (flatten (list
            (symbol->string (object-name node))
            (map (lambda (v) (string-append " " (graph-pretty-label v)))
                (filter (compose not struct?) (rest (vector->list (struct->vector node))))))))]
        [else (~v node)]))

;; graph-to-dot :: Any -> String
(define (graph-to-dot graph)
    (define nodes (graph-dfs (list graph)))
    (define node-index-mapping (enumerate-graph-nodes nodes))
    (define (node->name node)
        (string-append "N" (number->string (hash-ref node-index-mapping node))))
    (apply string-append (flatten (list
        "digraph {\n"
        (map (lambda (node)
            (define node-label (graph-pretty-label node))
            (string-append "  " (node->name node) " [label = \"" node-label "\"]\n"))
            nodes)
        (map (lambda (node)
            (define node-name (node->name node))
            (map (lambda (dep)
                (define dep-name (node->name dep))
                (string-append "  " node-name " -> " dep-name "\n"))
                (dependencies-of-object node)))
            nodes)

        "}\n"
    ))))

;; save-to-dot-file :: Any, String -> Void. [Fx]
(define (save-to-dot-file graph filename)
    (define dot (graph-to-dot graph))
    (define out (open-output-file filename #:exists 'truncate))
    (display dot out)
    (close-output-port out))

;; graph-to-svg :: Any, String -> Void. [Fx]
(define (graph-to-svg graph)
    (define script (graph-to-dot graph))
    (define-values (sp out in err) (subprocess #f #f #f (find-executable-path "dot") "-Tsvg"))
    (display script in)
    (flush-output in)
    (close-output-port in)
    (define result (port->string out))
    (displayln (port->string err))
    (close-input-port out)
    (close-input-port err)
    (subprocess-wait sp)
    result)

;; save-to-svg-file :: Any, String -> Void. [Fx]
(define (save-to-svg-file graph filename)
    (define svg (graph-to-svg graph))
    (define out (open-output-file filename #:exists 'truncate))
    (display svg out)
    (close-output-port out))
