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

; Environment

(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

; Interpreting with Environments

(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC expr
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (fdC-body fd)
                          ; This gives us dynamic scope
                          ;(extend-env (bind (fdC-arg fd)
                          ;                  (interp a env fds))
                          ;            env)
                          ; This gives us static scope
                          (extend-env (bind (fdC-arg fd)
                                            (interp a env fds))
                                      mt-env)
                          fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]))

(define (get-fundef [name : symbol] [fds : (listof FunDefC)]) : FunDefC
  (if (empty? fds)
      (error 'get-fundef "reference to undefined function")
      (if (symbol=? name (fdC-name (first fds)))
          (first fds)
          (get-fundef name (rest fds)))))

(define (lookup [for : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

; Tests

(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5))))
      15)

(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)

(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)

; An interesting test case that reveals errors
; with the initial implementation using environments
;
; It is only an error because it does produce the
; same results as our substitution based interpreter
;
; The incorrect implementation actually implements
; what is known as dynamic scope
;
; (interp (appC 'f1 (numC 3))
;         mt-env
;         (list (fdC 'f1 'x (appC 'f2 (numC 4)))
;               (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))

