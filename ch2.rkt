#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (parse [s : s-expression]) : ArithC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(parse '(+ (* 1 2) (+ 2 3)))

; If you forget to quote the argument to the parser then Racket would evaluate
; the argument reducing it to a number. Since number is not a s-expression a
; `typecheck failed` error would occur.

; For e.g.
; (parse (+ (* 1 2) (+ 2 3)))

; Interesting examples
; 1. (parse '(+ 1))
; 2. (parse '(+ 1 2 3))
