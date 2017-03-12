#lang plai-typed

(define-type MisspelledAnimal
  [caml (humps : number)]
  [yacc (height : number)])

(define ma1 : MisspelledAnimal (caml 2))
(define ma2 : MisspelledAnimal (yacc 1.9))

(define (good? [ma : MisspelledAnimal]) : boolean
  (type-case MisspelledAnimal ma
    [caml (h) (>= h 2)]
    [yacc (h) (> h 2.1)]))

; Without pattern matching
; (define (good? [ma : MisspelledAnimal]) : boolean
;   (cond
;     [(caml? ma) (>= (caml-humps ma) 2)]
;     [(yacc? ma) (> (yacc-height ma) 2.1)]))

(test (good? ma1) #t)
(test (good? ma2) #f)

; Answers to "Do Now!"
; You get `typecheck failed` errors when you mis-apply the functions.
