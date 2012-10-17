#lang plai

;; Exercise 3.4.1
;;
;; I don't think that the eager and lazy regimes will always produce the same answer
;; for the WAE language.
;;
;; Consider the following expression:
;;
;; {with {x y} 3}
;;
;; When evaluated eagerly we get an error "free identifier" since the y has no binding
;; instance in scope. However, when evaluated lazily since the x is never used in the
;; body we never try to evaluate the y and so 3 is returned.


;; Exercise 3.4.2
;;
;; Here is a counter-example that shows that the lazy regime can generate an answer in
;; fewer steps than the eager regime.
;;
;; {with {y {+ 1 2}}
;;   {with {x {+ y 3}}
;;     y}
;;
;; The eager evaluation gives:
;; 
;; {with {y {+ 1 2}} {with {x {+ y 3}} y} ; operation
;; {with {y 3} {with {x {+ y 3}} y}       ; substitution
;; {with {y 3} {with {x {+ 3 3}} 3}       ; descend
;; {with {x {+ 3 3}} 3}                   ; operation
;; {with {x 6} 3}                         ; substitution
;; {with {x 6} 3}                         ; descend
;; 3
;;
;; Eager takes 6 steps.
;;
;; The lazy evaluation gives:
;;
;; {with {y {+ 1 2}} {with {x {+ y 3}} y}            ; substitution
;; {with {y {+ 1 2}} {with {x {+ {+ 1 2} 3} {+ 1 2}} ; descend
;; {with {x {+ {+ 1 2} 3} {+ 1 2}}                   ; substitution
;; {with {x {+ {+ 1 2} 3} {+ 1 2}}                   ; descend
;; {+ 1 2}                                           ; operation
;; 3
;;
;; Lazy takes 5 steps.

;; Exercise 3.4.3
;;
;; I think it has something to do with lazy evaluation. Maybe because by using
;; lazy evaluation an identifier may not hold the value of an expression until
;; we explicitly need to use its value.