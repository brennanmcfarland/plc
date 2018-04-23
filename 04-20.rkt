; macros
; macros are compiler commands
; essentially a way of creating language syntax

;(needs to be racket)
#lang racket

; the long way
; creates a syntax
;` is a list that is going to be evaluated, the commas demarcate what gets evaluated
(define-syntax debug
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(begin (print ,(cadr slist)) ,(cadr slist))) ; print whatever the value of the command was and return it
    ))
; example:
;(define factorial (lambda (x)
;                      (if (zero? x)
;                          1
;                          (* x (debug (factorial (- x 1)))))))
;(factorial 3) -> 11121116

(define-syntax foreachdo
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(map (lambda (,(cadr slist)) ,(caddr slist)) ,(cadddr slist)))))
; example:
;(foreachdo x (* x x) '(1 2 3 4 5)) -> '(1 4 9 16 25)

(define-syntax just
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(list 'just ,(cadr slist)))))

(define-syntax nothing
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(list 'nothing))))

; the difference w/ syntax is it gets replaced at compile time