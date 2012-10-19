#lang plai

;; F1WAE concrete syntax
;;
;; <F1WAE> ::= <num>
;;          | {+ <F1WAE> <F1WAE>}
;;          | {with {<id> <F1WAE>} <F1WAE>}
;;          | <id>
;;          | {<id> <F1WAE>}

;; F1WAE abstract syntax

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)])

;; parse : sexp -> F1AE
;; to convert s-expressions into F1AEs

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(with) (with (first (second sexp))
                     (parse (second (second sexp)))
                     (parse (third sexp)))]
       [else (app (first sexp) (parse (second sexp)))])]))

(test (parse '5) (num 5))
(test (parse 'x) (id 'x))
(test (parse '{+ {+ 1 2} {+ 3 4}})
      (add (add (num 1) (num 2))
           (add (num 3) (num 4))))
(test (parse '{with {x 5} {+ x 6}})
      (with 'x (num 5) (add (id 'x) (num 6))))
(test (parse '{f 10})
      (app 'f (num 10)))

;; subst : F1AE symbol F1AE -> F1AE
;; substitutes second argument with third argument in first argument
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument

(define (subst expr sub-id val)
  (type-case F1WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
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
    [id (v) (if (symbol=? v sub-id) val expr)]
    
    ;; extends subst to handle the F1WAE language
    [app (f x) (app f (subst x sub-id val))]))

(test (subst (app 'f (add (id 'x) (num 1))) 'x (num 5))
      (app 'f (add (num 5) (num 1))))

;; A function definition has three parts:
;;
;; 1. the name of the function
;; 2. the name of its arguments (known as formal parameters)
;; 3. the function's body

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

;; lookup-fundef : symbol listof(FunDef) -> FunDef

(define (lookup-fundef fun-name fun-defs)
  (cond
    [(empty? fun-defs) (error fun-name "function not found")]
    [else (if (symbol=? fun-name (fundef-fun-name (first fun-defs)))
              (first fun-defs)
              (lookup-fundef fun-name (rest fun-defs)))]))

;; interp : F1WAE listof(FunDef) -> number
;; evaluates F1WAE expressions by reducing them to their corresponding
;; values

(define (interp expr fun-defs)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs) (interp r fun-defs))]
    [with (bound-id named-expr bound-body)
          (interp (subst bound-body
                         bound-id
                         (num (interp named-expr fun-defs)))
                  fun-defs)]
    [id (v) (error 'interp "free identifier")]
    [app (fun-name arg-expr)
         (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
           (interp (subst (fundef-body the-fun-def)
                          (fundef-arg-name the-fun-def)
                          (num (interp arg-expr fun-defs)))
                   fun-defs))]))

(test (interp (parse '{double {double 5}})
              (list (fundef 'double
                            'n
                            (add (id 'n) (id 'n)))))
      20)