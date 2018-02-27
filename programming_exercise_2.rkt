; TODO: is there some way to tell if the stack is blowing up?

; given a number and ordered list of numbers insert the number in the proper place
(define insert
  (lambda (num lis)
    (insert-cps num lis (lambda (v) v))))

(define insert-cps
  (lambda (num lis return)
    (cond
      ((null? lis) (return (cons num '())))
      ((> num (car lis)) (insert-cps num (cdr lis) (lambda (v) (return (cons (car lis) v)))))
      (else (return (cons num lis))))))

; given two ordered lists of numbers return the ordered combination of both lists
(define merge
  (lambda (lis1 lis2)
    (merge-cps lis1 lis2 (lambda (v) v))))

(define merge-cps
  (lambda (lis1 lis2 return)
    (cond
      ((null? lis1) (return lis2))
      ((null? lis2) (return lis1))
      ((< (car lis1) (car lis2)) (merge-cps (cdr lis1) lis2 (lambda (v) (return (cons (car lis1) v)))))
      (else (merge-cps lis1 (cdr lis2) (lambda (v) (return (cons (car lis2) v))))))))

; given a list of atoms, remove any atom that is a repeat the atom immediately preceding it
(define removedups
  (lambda (lat)
    (removedups-cps lat (lambda (v) v))))

(define removedups-cps
  (lambda (lat return)
    (cond
      ((null? lat) (return '()))
      ((null? (cdr lat)) (return lat))
      ((equal? (car lat) (car (cdr lat))) (removedups-cps (cdr lat) return))
      (else (removedups-cps (cdr lat) (lambda (v) (return (cons (car lat) v))))))))

; given a list, return the number of pairs of parentheses
(define numparens
  (lambda (lis)
    (numparens-cps lis (lambda (v) v))))

(define numparens-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return 1))
      ((list? (car lis)) (numparens-cps (car lis) (lambda (v1) (numparens-cps (cdr lis) (lambda (v2) (return (+ v1 v2)))))))
      (else (numparens-cps (cdr lis) return)))))

; given a list, duplicate its content including sublists
(define dup*
  (lambda (lis)
    (dup*-cps lis (lambda (v) v))))

(define dup*-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '()))
      ((list? (car lis)) (dup*-cps (car lis) (lambda (v1) (dup*-cps (cdr lis) (lambda (v2) (return (cons v1 (cons v1 v2))))))))
      (else (dup*-cps (cdr lis) (lambda (v) (return (cons (car lis) (cons (car lis) v)))))))))

; given a list, remove any atom that is a repeat of the atom immediately preceding it in the same sublist
(define removedups*
  (lambda (lis)
  (removedups*-cps lis (lambda (v) v))))

(define removedups*-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '()))
      ((list? (car lis)) (removedups*-cps (car lis) (lambda (v1) (removedups*-cps (cdr lis) (lambda (v2) (return (cons v1 v2)))))))
      ((and (not (null? (cdr lis))) (equal? (car lis) (cadr lis))) (removedups*-cps (cdr lis) (lambda (v) (return v))))
      (else (removedups*-cps (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; mergesort a list of numbers
; NOTE: it appears mergesort is a reserved word/built in function in Pretty Big, hence the underscore
(define merge_sort
  (lambda (lis)
    (merge_sort-cps lis (lambda (v) v))))

(define merge_sort-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '()))
      ((null? (cdr lis)) (return lis))
      (else (merge_sort-cps (car (split-cps lis (lambda (v1 v2) (cons v1 v2)))) (lambda (v1) (merge_sort-cps (cdr (split-cps lis (lambda (v1 v2) (cons v1 v2)))) (lambda (v2) (return (merge v1 v2)))))))))) 


; given a list of atoms, return a list that contains two lists of atoms:
; the first list contains odd-indexed atoms (starting from 1)
; the second list contains the even-indexed atoms (starting from 1)
; this is a helper function for merge_sort-cps
(define split-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '() '()))
      ((null? (cdr lis)) (return lis '()))
      (else (split-cps (cddr lis) (lambda (v1 v2) (return (cons (car lis) v1) (cons (cadr lis) v2))))))))


; given a list and a lat, return the first list with each atom from left to right replaced by the corresponding atom of the second list until the second list runs out of atoms
(define replaceatoms
  (lambda (lis lat)
    (replaceatoms-cps (lis lat (lambda (v) v)))))

(define replaceatoms-cps
  (lambda (lis lat return)
    (