#lang plai

;; AE - Arithmetic Expression
(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

;; Exercise 1.2.1
;;
;; The lhs and rhs sub-expressions are of type AE rather than of type
;; num to allow for expressions with multiple additions and subtractions.
;;
;; For example,
;;
;; (5 + 4) + (3 - 2) - 1
;; > (sub (add (add (num 5) (num 4))
;;             (sub (num 3) (num 2)))
;;        (num 1))
;;
;; Otherwise, we'd only be able to write expressions with one addition
;; or one subtraction.
;;
;; 1 + 2
;; > (add (num 1) (num 2))
;;
;; 5 - 4
;; > (sub (num 5) (num 4))