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

;; parse : sexp -> WAE
;; to convert s-expressions into WAEs

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(-) (sub (parse (second sexp))
                 (parse (third sexp)))]
       [(with) (with (first (second sexp))
                     (parse (second (second sexp)))
                     (parse (third sexp)))])]))

;; Definition 9 (Substitution, take 5) To substitute identifier i in e
;; with expression v, replace all free instances of i in e with v.

;; subst : WAE symbol WAE -> WAE
;; substitutes second argument with third argument in first argument
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument

(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              ;; expr
              (with bound-id
                    (subst named-expr sub-id val)
                    bound-body)
              ;; the above handles the case
              ;; {with {x 5}
              ;;   {with {x x}
              ;;     x}}
              (with bound-id
                    ;; named-expr
                    (subst named-expr sub-id val)
                    ;; the above handles the case
                    ;; {with {x 5}
                    ;;   {with {y x}
                    ;;     y}}
                    (subst bound-body sub-id val)))]
    [id (v) (if (symbol=? v sub-id) val expr)]))

;; calc : WAE -> number
;; evaluates WAE expressions by reducing them to numbers

(define (calc expr)
  (type-case WAE expr
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]
    [with (bound-id named-expr bound-body)
          (calc (subst bound-body
                       bound-id
                       (num (calc named-expr))))]
    [id (v) (error 'calc "free identifier")]))

(test (calc (parse '5)) 5)
(test (calc (parse '{+ 5 5})) 10)
(test (calc (parse '{with {x {+ 5 5}} {+ x x}})) 20)
(test (calc (parse '{with {x 5} {+ x x}})) 10)
(test (calc (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) 14)
(test (calc (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) 4)
(test (calc (parse '{with {x 5} {+ x {with {x 3} 10}}})) 15)
(test (calc (parse '{with {x 5} {+ x {with {x 3} x}}})) 8)
(test (calc (parse '{with {x 5} {+ x {with {y 3} x}}})) 10)
(test (calc (parse '{with {x 5} {with {y x} y}})) 5)
(test (calc (parse '{with {x 5} {with {x x} x}})) 5)