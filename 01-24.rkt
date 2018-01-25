; removeall*: remove all instances of atom x in a list that can contain sublists
; (removeall* 'x '(a b x (a x (((x x n b)) x c )))) => (a b (a (((n b)) c)))
(define removeall*
  (lambda (x lis)
    (cond
      ((null? lis) '())
      ((pair? (car lis)) (cons (removeall* x (car lis)) (removeall* x (cdr lis))))
      ((eq? x (car lis)) (removeall* x (cdr lis)))
      (else (cons (car lis) (removeall* x (cdr lis)))))))

; member*? is x a member of a list that can contain sublists?
(define member*?
  (lambda (x lis)
    (cond
      ((null? lis) #f)
      ((eq? x (car lis)) #t)
      ((list? (car lis)) (or (member*? x (car lis)) (member*? x (cdr lis))))
      (else (member*? x (cdr lis))))))

; (replaceall* 'x 'y '((x((x))))) => ((y ((y)))
; replace all x with y in lis
(define replaceall*
  (lambda (x y lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (replaceall* x y (car lis)) (replaceall* x y (cdr lis))))
      ((eq? x (car lis)) (cons y (replaceall* x y (cdr lis))))
      (else (cons (car lis) (replaceall* x y (cdr lis)))))))
             
; (empty* '(a (b (c)) (d)) => ((())())
; remove all non-() atoms from a list
(define empty*
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (empty* (car lis)) (empty* (cdr lis))))
      (else (empty* (cdr lis))))))

; (flatten '(a (b (c)) (d)) => (a b c d)
; flatten a nested list to just a list of atoms
(define flatten
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (append (flatten (car lis)) (flatten (cdr lis))))
      (else (cons (car lis) (flatten (cdr lis)))))))