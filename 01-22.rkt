; create a list of n copies of a
(define repeat
  (lambda (n a)
   (if (zero? n)
    '()
    (cons a (repeat (- n 1) a)))))

; remove the first instance of x in list lis
(define removefirst
  (lambda (x lis)
    (cond
      ((null? lis) '())
      ((eq? x (car lis)) (cdr lis))
      (else (cons (car lis) (removefirst x (cdr lis)))))))

; remove all instances of x in list lis
(define removeall
  (lambda (x lis)
    (cond
      ((null? lis) '())
      ((eq? x (car lis)) (removeall x (cdr lis)))
      (else (cons (car lis) (removeall x (cdr lis)))))))

; replace all x with y in list lis
(define replaceall
  (lambda (x y lis)
    (cond
      ((null? lis) '())
      ((eq? x (car lis)) (cons y (replaceall x y (cdr lis))))
      (else (cons (car lis) (replaceall x y (cdr lis)))))))

; return the squares of a list lis as a list
(define squares
  (lambda (lis)
    (if (null? lis)
        '()
        (cons (* (car lis) (car lis)) (squares (cdr lis))))))

; append list l1 to list l2
(define myappend
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (myappend (cdr l1) l2)))))

; reverse a list
(define myreverse
  (lambda (lis)
    (if (null? lis)
        '()
        (myappend (myreverse (cdr lis)) (cons (car lis '()))))))

; map a function onto a list
(define mymap
  (lambda (f lis)
    (if (null? lis)
        '()
        (cons (f (car lis)) (mymap f (cdr lis))))))