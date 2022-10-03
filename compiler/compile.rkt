#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86)

;; Registers used
(define rax 'rax)
(define rbx 'rbx) ; tmp used for type tag tests
(define r8  'r8)  ; scratch
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; argument for C call

;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Label 'entry)
        (compile-e e '())
        (Ret)
        (Label 'raise_error_align)
        (Sub rsp 8)
        (Jmp 'raise_error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)         (compile-value i)]
    [(Bool b)        (compile-value b)]
    [(Char c)        (compile-value c)]
    [(Eof)           (compile-value eof)]
    [(Var x)         (compile-variable x c)]
    [(Prim0 p)       (compile-prim0 p c)]
    [(Prim1 p e)     (compile-prim1 p e c)]
    [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
    ;; TODO: implement n-ary primitive +
    [(PrimN p es)    (compile-primN p es c)]
    [(If e1 e2 e3)   (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)   (compile-begin e1 e2 c)]
    ;; TODO: this only works for single variable binding,
    ;; make it work in general
    ;;[(Let (list x) (list e1) e2)
    ;; (compile-let1 x e1 e2 c)]
    ;; TODO: implement let, let*, case, cond
    [(Let xs es e)   (compile-let2 xs es e c)]
    [(Let* xs es e)  (compile-let2 xs es e c)]
    [(Case ev cs el) (compile-case ev cs el c)]
    [(Cond cs el)    (compile-cond cs el c)]))

(define (compile-cond e1 e2 c)
  (match e1 
   ['() (compile-e e2 c)]
   [(cons (Clause a1 a2) css) 
    (let ((l1 (gensym 'cond))
          (l2 (gensym 'cond)))
      (seq (compile-e a1 c) 
           (Cmp 'rax val-false)
           (Je l1)
           (compile-e a2 c)
           (Jmp l2)
           (Label l1)
           (compile-cond css e2 c)
           (Label l2)))]
    )
  )


(define (compile-case e cs e1 c)
  (match cs
    ['() (compile-e e1 c)]
    [(cons (Clause a1 a2) css)    
  (let ((l1 (gensym 'case))
        (l2 (gensym 'case)))
    (seq (compile-e e c)
         (compile-member e a1 e1 c)
         (Cmp rax val-true)
         (Je l1)
         (compile-e e1 c)
         (Jmp l2)
         (Label l1)
         (compile-e a2 c)
         (Label l2)
         ))]))

(define (compile-member e xs e1 c)
  (match xs
    ['() (compile-e e1 c)]
    [(cons y ys) (let ((l1 (gensym 'case))
                       (l2 (gensym 'case)))
                   (seq (Mov 'r10 (value->bits y))
                        (Cmp rax 'r10)
                        (Je l1)
                        (compile-member e ys e1 c)
                        (Label l1)
                        (Mov rax val-true)
                        (Label l2)))]))


(define (compile-case1 e cs e1 c)
  (match cs
    ['() (compile-e e1 c)]
    [(cons (Clause a1 a2) css) (compile-e a2 c)
     ]))


;; Id Expr Expr CEnv -> Asm
;; NOTE: this is specialized for a single variable binding
;; You should write another function for the general case
(define (compile-let1 x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))


(define (compile-let2 xs es e c)
  (match xs
    [(cons x xs) (match es 
       [(cons y ys) (seq (compile-e y c)
                         (Push rax)
                         (compile-let2 xs ys e (cons x c))
                         (Add rsp 8)
)])]
    ['() (seq (compile-e e c))]))
    

(define (compile-primN p es c)
   (match es
     ['() (Mov 'r10 (value->bits 0))]
     [(cons a b) (seq (compile-primN p b c)
                      (Push 'r10)
                      (compile-e a c)
                      (Pop 'r10)
                      (Add 'r10 rax)
                      (Mov rax 'r10))]))


;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (match p
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq (pad-stack c)
                     (Call 'read_byte)
                     (unpad-stack c))]
    ['peek-byte (seq (pad-stack c)
                     (Call 'peek_byte)
                     (unpad-stack c))]))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p c)))

;; Op1 CEnv -> Asm
(define (compile-op1 p c)
  (match p
    ['add1
     (seq (assert-integer rax c)
          (Add rax (value->bits 1)))]
    ['sub1
     (seq (assert-integer rax c)
          (Sub rax (value->bits 1)))]
    ['zero?
     (let ((l1 (gensym 'is_zero)))
       (seq (assert-integer rax c)
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['char?
     (let ((l1 (gensym 'is_char)))
       (seq (And rax mask-char)
            (Xor rax type-char)
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['char->integer
     (seq (assert-char rax c)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint c)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object?
     (let ((l1 (gensym 'is_eof)))
       (seq (Cmp rax val-eof)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['write-byte
     (seq (assert-byte c)
          (pad-stack c)
          (Mov rdi rax)
          (Call 'write_byte)
          (unpad-stack c)
          (Mov rax val-void))]

    ;; TODO: implement -, abs, integer?, boolean?, etc.
    ['-        (seq (assert-integer rax c)
                    (Mov 'rbx (value->bits 0))
                         (Sub 'rbx 'rax)
                         (Mov 'rax 'rbx))]
    ['abs      (let ((l1 (gensym 'abs)))
                 (seq (assert-integer rax c)
                    (Cmp 'rax (value->bits 0))
                    (Jg l1)
                    (Mov 'rbx (value->bits 0))
                    (Sub 'rbx 'rax)
                    (Mov 'rax 'rbx)
                    (Label l1)))]
    ['not      (let ((l1 (gensym 'not)))
                 (seq (Cmp 'rax val-false)
                 (Mov 'rax val-true)
                 (Je l1)
                 (Mov 'rax val-false)
                 (Label l1)))]
    ['integer? (let ((l1 (gensym 'is_int)))
       (seq (And rax mask-int)
            (Xor rax type-int)
            (Cmp rax 0)
            (Mov rax val-true)
            (Je l1)
            (Mov rax val-false)
            (Label l1)))]
    ['boolean? (let ((l1 (gensym 'truefalse))
                     (l2 (gensym 'other)))
       (seq (Cmp rax val-true)
            (Je l1)
            (Cmp rax val-false)
            (Je l1)
            (Mov rax val-false)
            (Jmp l2)
            (Label l1)
            (Mov rax val-true)
            (Label l2)))]
    ))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p c)))

;; Op2 CEnv -> Asm
(define (compile-op2 p c)
  (match p
    ['+
     (seq (Pop r8)
          (assert-integer r8 c)
          (assert-integer rax c)
          (Add rax r8))]
    ['-
     (seq (Pop r8)
          (assert-integer r8 c)
          (assert-integer rax c)
          (Sub r8 rax)
          (Mov rax r8))]))



;; HINT: Another potentially helpful function that
;; emits code to execute each expression and push
;; all the values on to the stack, analogous to interp*-env

;; [Listof Expr] CEnv -> Asm
(define (compile-e* es c)
  (match es
    ['() (seq)]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-e* es (cons #f c)))]))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))







;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))




;; CEnv -> Asm
;; Pad the stack to be aligned for a call
(define (pad-stack c)
  (match (even? (length c))
    [#t (seq (Sub rsp 8))]
    [#f (seq)]))

;; CEnv -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack c)
  (match (even? (length c))
    [#t (seq (Add rsp 8))]
    [#f (seq)]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

(define (assert-type mask type)
  (λ (arg c)
    (seq (Mov rbx arg)
         (And rbx mask)
         (Cmp rbx type)
         (Jne (error-label c)))))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))

(define (assert-codepoint c)
  (let ((ok (gensym)))
    (seq (assert-integer rax c)
         (Cmp rax (value->bits 0))
         (Jl (error-label c))
         (Cmp rax (value->bits 1114111))
         (Jg (error-label c))
         (Cmp rax (value->bits 55295))
         (Jl ok)
         (Cmp rax (value->bits 57344))
         (Jg ok)
         (Jmp (error-label c))
         (Label ok))))

(define (assert-byte c)
  (seq (assert-integer rax c)
       (Cmp rax (value->bits 0))
       (Jl (error-label c))
       (Cmp rax (value->bits 255))
       (Jg (error-label c))))

;; CEnv -> Label
;; Determine correct error handler label to jump to.
(define (error-label c)
  (match (even? (length c))
    [#t 'raise_error]
    [#f 'raise_error_align]))
