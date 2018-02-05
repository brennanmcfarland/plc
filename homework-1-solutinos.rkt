; these are just class notes, the real solutions are posted online
; removedups
(define removedups
  (lambda (l)
    (cond
      ((null? l) l)
      ((null? (cdr l)) l)
      ((eq? (car l) (car (cdr l))) (removedups (cdr l)))
      (else (cons (car l) (removedups (cdr l)))))))

;removedups*
(define removedups*
  (lambda (l)
    (cond
      ((null? l) l)
      ((list? (car l)) (cons (removedups* (car l)) '(removedups* (cdr l))))
      ((null? (cdr l)) l)
      ((eq? (car l) (cadr l)) (removedups* (cdr l)))
      (else (cons (car l) (removedups* (cdr l)))))))

;removedups**
(define removedups**
  (lambda (l)
    (cond
      ((null? l) l)
      ((and (list? (car l)) (null? (cdr l))) (cons (removedups** (car l)) '()))
      ((and (list? (car l)) (list? (cadr l))) (equal? (removedups** (car l)) (removedups** (cadr l)))) (removedups*** (cdr l)))
      ((list? (car l)) (cons (removedups** (car l)) (removedups** (cdr l))))
      ((null? (cdr l)) l)
      ((eq? (car l) (cadr l)) (removedups** (cdr l)))
      (else (cons (car l) (removedups** (cdr l)))))))

;split
(define split
  (lambda (l)
    (cond
      ((null? l) '(()()))
      ((null? (cdr l)) (list (cons (car l) (car (split (cdr l)))) (cadr (split (cdr l)))))
      (else (list (cons (car l) (car (split (cddr l)))) (cons (cadr l) (cadr (split (cddr l)))))))))

;split*
(define split*
  (lambda (l)
    (cond
      ((null? l) '(()()))
      ((and (null? (cdr l)) (list? (car l))) (list (cons (split* (car l)) (cadr (split* (cdr l)))) (cadr (split* (cdr l)))))
      ((null? (cdr l)) (list (cons (car l) (car (split* (cdr l)))) (cadr (split* (cdr l)))))
      ((and (list? (car l)) (list? (cadr l))) (list (cons (split* (car l)) (car (split* (cddr l)))) (cons (cadr (split* (cadr l)) (cadr (split* (cddr l)))))))
      ((list? (car l)) (list (cons (split* (car l)) (car (split* (cddr l)))) (cons (cadr l) (cadr (split* (cddr l))))))
      ((list? (cadr l)) (list (cons (car l) (car (split* (cddr l)))) (cons (split* (cadr l)) (cadr (split* (cddr l))))))
      (else (list (cons (car l) (car (split* (cddr l)))) (cons (cadr l) (cadr (split* (cddr l)))))))))