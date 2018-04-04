; append two lists
(define myappend
  ((lambda (m)
     (m m))
   (lambda (a)
     (lambda (l1 l2)
       (if (null? l1)
           l2
           (cons (car l1) ((a a) (cdr l1) l2)))))))

; reverse a list, uses append as a helper
(define myreverse
  ((lambda (m n)
     (m m n))
   (lambda (r a)
     (lambda (l)
       (if (null? l)
           '()
           ((a a) ((r r a) (cdr l)) (cons (car l) '())))))
   (lambda (a)
     (lambda (l1 l2)
       (if (null? l1)
           l2
           (cons (car l1) ((a a) (cdr l1) l2)))))))

; merge
(define mymerge
  ((lambda (makem)
     (makem makem))
   (lambda (m)
     (lambda (l1 l2 return)
       (cond
         ((null? l1) (return l2))
         ((null? l2) (return l1))
         ((< (car l1) (car l2)) ((m m) (cdr l1) l2 (lambda (v) (return (cons (car l1) v)))))
         (else ((m m) l1 (cdr l2) (lambda (v) (return (cons (car l2) v))))))))))
     

; split
(define mysplit
  ((lambda (makes)
     (makes makes))
   (lambda (s)
     (lambda (lis return)
       (cond
         ((null? lis) (return '() '()))
         ((null? (cdr lis)) (return lis '()))
         (else ((s s) (cddr lis) (lambda (v1 v2) (return (cons (car lis) v1) (cons (cadr lis) v2))))))))))

; mergesort
(define mymergesort
  ((lambda (m s g)
     (m m s g))
   (lambda (m s g)
     (lambda (lis return)
       (cond
         ((null? lis) (return '()))
         ((null? (cdr lis)) (return lis))
         (else ((m m s g) (car ((s s) lis (lambda (v1 v2) (cons v1 v2)))) 
(lambda (v1) ((m m s g) (cdr ((s s) lis (lambda (v1 v2) (cons v1 v2))))
 (lambda (v2) (return ((g g) v1 v2))))))))))
   (lambda (s)
     (lambda (lis return)
       (cond
         ((null? lis) (return '() '()))
         ((null? (cdr lis)) (return lis '()))
         (else ((s s) (cddr lis) (lambda (v1 v2) (return (cons (car lis) v1) (cons (cadr lis) v2)))))))
     )
   (lambda (g)
     (lambda (m)
     (lambda (l1 l2 return)
       (cond
         ((null? l1) (return l2))
         ((null? l2) (return l1))
         ((< (car l1) (car l2)) ((m m) (cdr l1) l2 (lambda (v) (return (cons (car l1) v)))))
         (else ((m m) l1 (cdr l2) (lambda (v) (return (cons (car l2) v))))))))
     )))

; not very functional
(define fastmult
  (lambda (l)
    (letrec ((loop (lambda (l break return)
                     (cond
                       ((null? l) (return 1))
                       ((zero? (car l)) (break 0))
                       (else (loop (cdr l) break (lambda (v) (return (* (car l) v)))))))))
             (loop l (lambda (v) v) (lambda (v) v)))))

; functional
(define fastmult2
  (lambda (l)
    (((lambda (m)
       (m m))
     (lambda (loop)
       (lambda (l break return)
                     (cond
                       ((null? l) (return 1))
                       ((zero? (car l)) (break 0))
                       (else ((loop loop) (cdr l) break (lambda (v) (return (* (car l) v)))))))))
    l (lambda (v) v) (lambda (v) v))))

(define replaceall
  (lambda (x y l)
    (cond
      ((null? l) l)
      ((eq? (car l) x) (cons y (replaceall x y (cdr l))))
      (else (cons (car l) (replaceall x y (cdr l)))))))

; currying: turning multiple inputs into one input
(define curry3
  (lambda (f)
    (lambda (x)
      (lambda (y)
        (lambda (z)
          (f x y z))))))

(define uncurry3
  (lambda (f)
    (lambda (a b c)
      (((f a) b) c))))