#lang racket
(provide interp interp-env)
(require "ast.rkt" "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type REnv = (Listof (List Id Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof) eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    ;; TODO: implement n-ary primitive +
    [(PrimN p es) (match es
                    ['err 'err]
                    ['() 0]
                    [(list x xs ...) (interp-prim2 p (interp-env x r) (interp-env (PrimN p xs) r))])]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    ;; TODO: implement cond
    [(Cond cs e) (if (interp-first cs r) (interp-second cs r) (interp-env e r))]
    ;; TODO: implement case
    [(Case ev cs el) (if (interp-case1 ev cs) (interp-case2 ev cs) (interp el))]
    ;; TODO: this works for just a single binding
    ;; but you need to make it work in general
    ;;[(Let (list x) (list e1) e2)
    ;; (match (interp-env e1 r)
    ;;   ['err 'err]
    ;;   [v (interp-env e2 (ext r x v))])]
    ;; TODO: implement let, let*
    [(Let  xs es e)
     (interp-let xs es e r)]
    [(Let* xs es e) (interp-let xs es e r)]
    ))


(define (interp-let xs es e r)
  (match xs
       ['() (interp-env e r)]
       [(cons x xs) (match es
                      ['() (interp-env e r)]
                      [(cons y ys) (match (interp-env y r)
                                     ['err 'err]
                                     [v (interp-let xs ys e (ext r x v))])])]))


(define (interp-first cs r)
  (match cs
    [(list (Clause e1 e2) xs ...)  (if (interp-env e1 r) #t (interp-first xs r))]
    [_ #f]))

(define (interp-second cs r)
  (match cs
    [(list (Clause e1 e2)) (interp-env e2 r)]
    [(list (Clause e1 e2) xs ...) (if (interp-env e1 r) (interp-env e2 r) (interp-second xs r))]))



(define (interp-case1 e cs)
  (match cs
    [(list (Clause e1 e2) xs ...)  (if (member (interp e) e1) #t (interp-case1 e xs))]
    [_ #f]))
  

(define (interp-case2 e cs)
  (match cs
    [(list (Clause e1 e2)) (interp e2)]
    [(list (Clause e1 e2) xs ...) (if (member (interp e) e1) (interp e2) (interp-case2 e xs))]
    [_ (error "wtf happened here when interpreting case")]))



;; HINT: this is a function that may come in handy.
;; It takes a list of expressions and environment
;; and evaluates each expression in order.  If any
;; expression produces 'err, the whole thing produces
;; 'err; otherwise it produces a list of values.

;; type Answer* = 'err | [Listof Value]
;; [Listof Expr] Env -> Answer*
(define (interp*-env es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (match (interp*-env es r)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))

;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))

