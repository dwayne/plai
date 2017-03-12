#lang plai-typed

; Core language

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

; Surface language

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]

  ; 4.2: Unary negation
  [uminusS (e : ArithS)]
  
  [multS (l : ArithS) (r : ArithS)])

; Step 1: Modify parse to return ArithS terms
(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(-) (if (= (length sl) 2) ; 4.2: Determine between unary/binary minus
                  (uminusS (parse (second sl)))
                  (bminusS (parse (second sl)) (parse (third sl))))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

; (parse '(+ (* 1 2) (- 2 3)))
; (parse '(- 5))
; (parse '(- (+ (- 5 3) (- 6))))

; Step 2: Translate ArithS values into ArithC
(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    ; l - r = l + -1 x r
    ; The following doesn't work because it returns an ArithS
    ; and not an ArithC. To make it work we'd have to call
    ; desugar on it.
    ; [bminusS (l r) (plusS l (multS (numS -1) r))]

    ; Just do the first level of conversions here one time and
    ; remember to desugar l and r.
    ; NOTE: If you forget to desugar l (for e.g.) then you'd be
    ; passing an ArithS to plusC which takes an ArithC. We'd get
    ; a type check error.
    [bminusS (l r) (plusC (desugar l)
                         (multC (numC -1)
                                (desugar r)))]
    ; 4.2: Unary negation
    ;
    ; -e = 0 - e
    ;
    ; It works but there are two problems:
    ;
    ; 1. The recursion is generative.
    ; 2. We are implicitly depending on what bminusS means and if
    ;    its meaning changes, so will that of uminusS, even if we
    ;    don't want it to.
    ; [uminusS (e) (desugar (bminusS (numS 0) e))]

    ; A better alternative
    ;
    ; -e = -1 x e
    [uminusS (e) (multC (numC -1)
                        (desugar e))]))

; NOTE: If we attempt to fix the generative recursive problem
; by doing (desugar e) within the uminusS definition of
; -e = 0 - e then we'd get a type check error since we'd
; be passing an ArithC to bminusS.

(test
 (interp (desugar(uminusS (plusS (bminusS (numS 5) (numS 3))
                                 (uminusS (numS 6))))))
 4)