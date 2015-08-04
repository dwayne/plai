#lang plai-typed

; I add a conditional that treats 0 as false
; and everything else as true.

; Core language

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  ; conditional
  [ifC (test-expr : ExprC) (then-expr : ExprC) (else-expr : ExprC)])

(define (interp [a : ExprC]) : number
  (type-case ExprC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    [ifC (test-expr then-expr else-expr)
         (if (not (= (interp test-expr) 0)) ; 0 is false
             (interp then-expr)
             (interp else-expr))]))

(test
 (interp (ifC (numC 0) (numC 1) (numC 2)))
 2)

(test
 (interp (ifC (numC 1) (numC 1) (numC 2)))
 1)