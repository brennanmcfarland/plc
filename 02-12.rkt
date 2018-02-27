; Tail Recursion

; accumulator passing style

(define len
  (lambda (lis)
    (if (null? lis)
        0
        (+ 1 (len (cdr lis))))))

(define len-acc
  (lambda (lis acc)
    (if (null? lis)
        acc
        (len-acc (cdr lis) (+ 1 acc)))))

;count the number of numbers in the list
(define countnums
  (lambda (lis)
    (cond
      ((null? lis) 0)
      ((number? (car lis)) (+ 1 (countnums (cdr lis))))
      (else (countnums (cdr lis))))))

(define countnums-acc
  (lambda (lis acc)
    (cond
      ((null? lis) acc)
      ((number? (car lis)) (countnums-acc (cdr lis) (+ 1 acc)))
      (else (countnums-acc (cdr lis) acc)))))

; continuation passing style

(define countnums-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return 0))
      ((number? (car lis)) (countnums-cps (cdr lis) (lambda (v) (return (+ 1 v)))))
      (else (countnums-cps (cdr lis) return)))))

(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

(define factorial-cps
  (lambda (n return)
    (if (zero? n)
        (return 1)
        (factorial-cps (- n 1) (lambda (v) (return (* n v)))))))

; myappend-cps
(define myappend-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (myappend-cps (cdr l1) l2 (lambda (v) (return (cons (car l1) v)))))))

; removeall-cps
(define removeall-cps
  (lambda (x lis return)
    (cond
      ((null? lis) (return '()))
      ((eq? x (car lis)) (removeall-cps x (cdr lis) return)) ; this is not returned because it needs to stay the way it is
      (else (removeall-cps x (cdr lis) (lambda (v) (return (cons (car lis) v))))))))
      
; squares-cps
; reverse-cps
(define reverse-cps
  (lambda (lis return)
    (if (null? lis)
        (return '())
        ...