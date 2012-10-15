#lang plai

;; AE - Arithmetic Expression
;;
;; A specification of the abstract syntax of AE.

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

;; A specification of the concrete syntax of AE
;; using BNF notation.
;;
;; <AE> ::= <num>
;;        | {+ <AE> <AE>}
;;        | {- <AE> <AE>}

;; parse : sexp -> AE
;; to convert s-expressions into AEs

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(-) (sub (parse (second sexp))
                 (parse (third sexp)))])]))

;; calc : AE -> number
;; consumes an AE and computes the corresponding number

(define (calc an-ae)
  (type-case AE an-ae
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]))

(test (calc (parse '3)) 3)
(test (calc (parse '{+ 3 4})) 7)
(test (calc (parse '{+ {- 3 4} 7})) 6)
