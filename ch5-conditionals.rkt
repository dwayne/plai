#lang plai-typed

; TODO: Return to this exercise to improve my solution.

; I'm not satisfied with my solution to this exercise.

; Add a boolean datatype.
(define-type BooleanT
  [boolT (b : boolean)])
; Should we define other boolean operations here?
; For e.g. and, or, not
;
; What about >, >=, <, <=? These operations can
; take ExprC but will result in BooleanT.
;
; I'm thinking we would have to define a
; sub-interpreter just for booleans.

; Core language

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  ; conditional
  [ifC (test-expr : BooleanT) (then-expr : ExprC) (else-expr : ExprC)])

(define (interp [a : ExprC]) : number
  (type-case ExprC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    [ifC (test-expr then-expr else-expr)
         (if (boolT-b test-expr) ; what happens when we change the definition?
             (interp then-expr)
             (interp else-expr))]))

(test
 (interp (ifC (boolT #f) (numC 1) (numC 2)))
 2)

(test
 (interp (ifC (boolT #t) (numC 1) (numC 2)))
 1)