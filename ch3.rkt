#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

; The "meaning" of addition and multiplication in this language is the addition
; and multiplication of the Racket number type.

; To change it so that the number had signed 32-bit/64-bit arithmetic I'd have
; to:
;
; 1. Use the `racket/fixnum` package
; 2. Use `fixnum` in place of `number` in the type definition
; 3. Use `fx+` and `fx*` for addition and multiplication respectively

(test (interp (numC 1)) 1)
(test (interp (plusC (numC 1) (numC 2))) 3)
(test (interp (multC (numC 2) (numC 3))) 6)
(test (interp (plusC (numC 1)
                     (multC (numC 3) (numC 4))))
      13)
