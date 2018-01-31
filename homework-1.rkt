; Brennan McFarland
; Jan 31, 2018
; Homework 1

; given a number and a list of numbers in order, insert the number in its proper place
;TODO: what about a zero length list?  should it return empty or the number?
(define insert
  (lambda (num lis)
    (cond
      ((null? lis) '())
      ((> num (car lis)) (cons (car lis) (insert num (cdr lis))))
      (else (cons num lis))
      )
    )
  )

; given two lists of numbers that in order, return the combination of both lists in order as a list
(define merge
  (lambda (lis1 lis2)
    (cond
      ((null? lis1) lis2)
      ((null? lis2) lis1)
      ((< (car lis1) (car lis2)) (cons (car lis1) (merge (cdr lis1) lis2)))
      (else (cons (car lis2) (merge lis1 (cdr lis2))))
      )
    )
  )

; given a list of atoms, remove any atom that is a repeat of the atom that immediately precedes it
(define removedups
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((null? (cdr lat)) lat)
      ((equal? (car lat) (car (cdr lat))) (removedups (cdr lat)))
      (else (cons (car lat) (removedups (cdr lat))))
      )
    )
  )

; given a list of atoms, return a list that contains two lists of atoms:
; the first list contains odd-indexed atoms (starting from 1)
; the second list contains the even-indexed atoms (starting from 1)
;TODO: not done, doesn't work right
(define split
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((null? (cdr lat)) (cons (car lat) '()))
      ((null? (cdr (cdr lat))) (cons (split (cons (car lat) '())) (split (cons (cdr lat) '()))))
      (else (cons (cdr (split (cdr lat))) (cons (car (split (cdr lat))) (car lat))))
      ;((null? (car (car lat))) (cons '() '())) ;this isn't right, fix it
      ;(else (cons (cons (car lat) (split (cdr lat))) (cons (car (cdr lat)) (split (cdr (cdr lat))))))
      )
    )
  )

; given an element and a (possibly nested) list, place the element in front of the leftmost
; element non-list atom, as deep in the sublist as needed.  If the leftmost atom is the empty list,
; place the element inside the empty list
(define deepcons
  (lambda (elm lis)
    (cond
      ((null? lis) (cons elm '()))
      ((list? (car lis)) (cons (deepcons elm (car lis)) (cdr lis)))
      (else (cons elm lis))
      )
    )
  )

; given a list, return the number of pairs of parentheses
(define numparens
  (lambda (lis)
    (cond
      ((null? lis) 1)
      ((list? (car lis)) (+ (numparens (car lis)) (numparens (cdr lis))))
      (else (numparens (cdr lis)))
      )
    )
  )

; given a list, duplicate all contents, including any sublists
;TODO: what about just the empty list?
(define dup*
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (cons (dup* (car lis)) (cons (dup* (car lis)) '())) (dup* (cdr lis))))
      (else (cons (car lis) (cons (car lis) (dup* (cdr lis)))))
      )
    )
  )

; given a (possibly nested) list, remove any atom that is the repeat of the atom that immediately
; precedes it in the same sublist
;(define removedups*
;  )

; given a (possibly nested) list, return a list containing two lists:
; the first list contains the odd-indexed elements (starting from 1)
; the second list contains the even-indexed elements (starting from 1)
; However, if any of these elements are also lists, these elements should be split as well
;(define split*
;  )

; given a (possibly nested) list, remove any element that, once repeated elements have been removed
; from it, is the repeat of any element (also once elements have been removed from it) that
; immediately precedes it in the same sublist
;(define removedups**
;  )