#lang plai-typed

; Core language

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

; Examples

; (define (double x) (+ x x))
(fdC 'double 'x (plusC (idC 'x) (idC 'x)))

; (define (quadruple x) (double (double x)))
(fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))

; (define (const5 _) 5)
(fdC 'const5 '_ (numC 5))

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'interp "free identifier")]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  ; Lazy
                  ; (interp (subst a
                  ;                (fdC-arg fd)
                  ;                (fdC-body fd))
                  ;        fds))]
                  ; Eager
                  (interp (subst (numC (interp a fds))
                                 (fdC-arg fd)
                                 (fdC-body fd))
                          fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]))

(define (get-fundef [name : symbol] [fds : (listof FunDefC)]) : FunDefC
  (if (empty? fds)
      (error 'get-fundef "reference to undefined function")
      (if (symbol=? name (fdC-name (first fds)))
          (first fds)
          (get-fundef name (rest fds)))))

(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]))

(test (subst (numC 5) 'x (plusC (idC 'x) (idC 'x)))
      (plusC (numC 5) (numC 5)))

; Example of how substitution currently works
; (subst (plusC (numC 1) (numC 2))
;        'x
;        (plusC (idC 'x) (idC 'x)))
; =>
; (plusC (plusC (numC 1) (numC 2))
;        (plusC (numC 1) (numC 2)))
; i.e. Argument evaluation is deferred
; so it means our substitution has lazy semantics

(define functions
  (list
   (fdC 'double 'x (plusC (idC 'x) (idC 'x)))
   (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
   (fdC 'const5 '_ (numC 5))))

(test (interp (appC 'double (numC 5)) functions)
      10)

(test (interp (appC 'quadruple (appC 'const5 (numC 1))) functions)
      20)

(test (interp (appC 'double
                    (plusC (numC 3)
                           (multC (numC 4) (numC 5))))
              functions)
      46)

; NOTE: A bit difficult to test the difference between
; lazy and eager semantics with just arithmetic expressions.