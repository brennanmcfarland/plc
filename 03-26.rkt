(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

(define crash
  (lambda (n)
    (error "Crashed!")))

; generalized form of recursion
(define factorial1
  (lambda (n)
    (if (zero? n)
        1
        (* n ((lambda (n)
                (if (zero? n)
                    1
                    (* n ((lambda (n)
                           (if (zero? n)
                               1
                               (* n (crash (- n 1)))))
                       (- n 1)))))
              (- n 1))))))

(define factorial2
  ((lambda (fact)
    (lambda (n)
      (if (zero? n)
          1
          (* n (fact (- n 1))))))
   ((lambda (fact)
      (lambda (n)
        (if (zero? n)
            1
            (* n (fact (- n 1))))))
    ((lambda (fact)
       (lambda (n)
         (if (zero? n)
             1
             (* n (fact (- n 1))))))
     ((lambda (fact)
        (lambda (n)
          (if (zero? n)
              1
              (* n (fact (- n 1))))))
      crash)))))

(define factorial3
  ((lambda (mkfact)
     (mkfact (mkfact (mkfact (mkfact crash)))))
   (lambda (fact)
     (lambda (n)
       (if (zero? n)
           1
           (* n (fact (- n 1))))))))

; allows for arbitrary number of recursions, don't have to manually unroll the loop
(define factorial4
  ((lambda (mkfact)
     (mkfact mkfact))
   (lambda (fact)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((fact fact) (- n 1))))))))

(define myappend
  ((lambda (mkapnd)
     (mkapnd mkapnd))
   (lambda (apnd)
     (lambda (l1 l2)
       (if (null? l1)
           l2
           (cons (car l1) ((apnd apnd) (cdr l1) l2)))))))

(define removeall*
  ((lambda (mkrmall)
     (mkrmall mkrmall))
   (lambda (rmall)
     (lambda (x lis)
       (cond
         ((null? lis) '())
         ((list? (car lis)) (cons ((rmall rmall) x (car lis)) ((rmall rmall) x (cdr lis))))
         ((eq? (car lis) x) ((rmall rmall) x (cdr lis)))
         (else (cons (car lis) ((rmall rmall) x (cdr lis)))))))))6
  