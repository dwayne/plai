#lang plai

;; A specification of the concrete syntax of WAE
;; using BNF notation.
;;
;; <WAE> ::= <num>
;;         | {+ <WAE> <WAE>}
;;         | {- <WAE> <WAE>}
;;         | {with {<id> <WAE>} <WAE>}
;;         | <id>

;; WAE - With with Arithmetic Expression
;;
;; A specification of the abstract syntax of WAE.

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)])