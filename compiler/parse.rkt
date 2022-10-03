#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?)              (Int s)]
    [(? boolean? s)            (Bool s)]
    [(? char? s)               (Char s)]
    ['eof                      (Eof)]
    [(? symbol? s)             (Var s)]
    [(list 'not e)             (If (parse e) (Bool #f) (Bool #t))]    
    [(list (? op0? o))         (Prim0 o)]
    [(list (? op1? o) e)       (Prim1 o (parse e))]
    [(list (? op2? o) e1 e2)   (Prim2 o (parse e1) (parse e2))]
    [(cons (? opN? o) es)      (PrimN o (map parse es))]
    [(list 'begin e1 e2)       (Begin (parse e1) (parse e2))]      
    [(list 'if e1 e2 e3)       (If (parse e1) (parse e2) (parse e3))]
    [(list 'let  bs e)         (parse-let  bs e)]
    [(list 'let* bs e)         (parse-let* bs e)]
    [(cons 'cond cs)           (parse-cond cs)]
    [(cons 'case (cons ev cs)) (parse-case ev cs)]
    [_                         (error "Parse error" s)]))

;; Any -> Boolean
(define (op0? x)
  (memq x '(read-byte peek-byte)))
(define (op1? x)
  (memq x '(add1 sub1 zero? char? integer->char char->integer
                 write-byte eof-object?
                 - abs integer? boolean?)))
(define (op2? x)
  (memq x '(-)))
(define (opN? x)
  (memq x '(+)))

;; S-Expr S-Expr -> Let
(define (parse-let bs e)
  (match bs
    ['() (Let '() '() (parse e))]
    [(cons (list (? symbol? x1) e1) bs)
     (match (parse-let bs e)
       [(Let xs es e)
        (Let (cons x1 xs) (cons (parse e1) es) e)])]
    [else (error "parse error")]))

;; S-Expr S-Expr -> Let
(define (parse-let* bs e)
  (match bs
    ['() (Let* '() '() (parse e))]
    [(cons (list (? symbol? x1) e1) bs)
     (match (parse-let* bs e)
       [(Let* xs es e)
        (Let* (cons x1 xs) (cons (parse e1) es) e)])]
    [else (error "parse error")]))

;; S-Expr -> Cond
(define (parse-cond cs)
  (match cs
    [(list (list 'else e)) (Cond '() (parse e))]
    [(cons (list p e) css)
     (match (parse-cond css)
       [(Cond cs el)
        (Cond (cons (Clause (parse p) (parse e)) cs) el)])]
    [_ (error "parse error")]))    

;; S-Expr S-Expr -> Case
(define (parse-case ev cs)
  (match cs
    [(list (list 'else e)) (Case (parse ev) '() (parse e))]
    [(cons (list ds e) css)
     (match (parse-case ev css)
       [(Case ev cs el)
        (Case ev (cons (Clause (parse-datums ds) (parse e)) cs) el)])]
    [_ (error "parse error")]))

;; S-Expr -> [Listof Datum]
(define (parse-datums ds)
  (match ds
    ['() '()]
    [(cons (? integer? i) ds)
     (cons i (parse-datums ds))]
    [(cons (? boolean? b) ds)
     (cons b (parse-datums ds))]
    [(cons (? char? c) ds)
     (cons c (parse-datums ds))]
    [(cons 'eof ds)
     (cons eof (parse-datums ds))]
    [_ (error "parse error")]))
