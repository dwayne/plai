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

(test (parse '3) (num 3))
(test (parse '{+ 3 4}) (add (num 3) (num 4)))
(test (parse '{+ {- 3 4} 7}) (add (sub (num 3) (num 4)) (num 7)))