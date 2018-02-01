; Brennan McFarland
; Jan 31, 2018
; Homework 1

; given a number and a list of numbers in order, insert the number in its proper place
(define insert
  (lambda (num lis)
    (cond
      ((null? lis) (cons num '()))
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
(define split
  (lambda (lat)
    (cond
      ((null? lat) '(() ())) ;no elements
      ((not (list? (car lat))) (split (cons lat '(())))) ;if it's a lat
      ((null? (cdr (car lat))) lat) ;one element
      (else (cons
             (cons (car (car lat)) (car (split (cdr (cdr (car lat)))))) ;odds
             (cons (cons (car (cdr (car lat))) (car (cdr (split (cdr (cdr (car lat))))))) '()) ;evens
             ))
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
(define dup*
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (dup* (car lis)) (cons (dup* (car lis)) (dup* (cdr lis)))))
      (else (cons (car lis) (cons (car lis) (dup* (cdr lis)))))
      )
    )
  )

; given a (possibly nested) list, remove any atom that is the repeat of the atom that immediately
; precedes it in the same sublist
(define removedups*
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (removedups* (car lis)) (removedups* (cdr lis))))
      ; if the list has at least two elements and the first two match
      ((and (not (null? (cdr lis))) (equal? (car lis) (car (cdr lis)))) (removedups* (cdr lis)))
      ; otherwise, check the rest of the list
      (else (cons (car lis) (removedups* (cdr lis))))
      )
    )
  )

; given a (possibly nested) list, return a list containing two lists:
; the first list contains the odd-indexed elements (starting from 1)
; the second list contains the even-indexed elements (starting from 1)
; However, if any of these elements are also lists, these elements should be split as well
(define split*
  (lambda (lis)
    (cond
      ((null? lis) '(() ()))
      ((not (list? lis)) lis)
      (else (split*_helper (cons lis '(()))))
      )
    )
  )

;a helper function, I had no choice
;splits the list when it's in the form ((odds + unprocessed) (evens))
(define split*_helper
  (lambda (lis)
    (cond
      ((not (list? (car lis))) (split* (cons lis '(())))) ;if it's a lat
      ((null? (cdr (car lis))) lis) ;one element
      (else (cons
             (cons (split* (car (car lis))) (car (split* (cdr (cdr (car lis)))))) ;odds
             (cons (cons (split* (car (cdr (car lis)))) (car (cdr (split* (cdr (cdr (car lis))))))) '()) ;evens
             ))
      )
    )
  )

; given a (possibly nested) list, remove any element that, once repeated elements have been removed
; from it, is the repeat of any element (also once elements have been removed from it) that
; immediately precedes it in the same sublist
(define removedups**
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (removedups* (car lis)) (removedups* (cdr lis))))
      ; if the list has at least two elements and the first two match
      ((and (not (null? (cdr lis))) (equal? (car lis) (car (cdr lis)))) (removedups* (cdr lis)))
      ; otherwise, check the rest of the list
      (else (cons (car lis) (removedups* (cdr lis))))
      )
    )
  )