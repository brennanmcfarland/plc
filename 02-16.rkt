; still multiplies everything by 0 as it pops the call stack
(define multiply
  (lambda (lis)
    (cond
      ((null? lis) 1)
      ((zero? (car lis)) 0)
      (else (* (car lis) (multiply (cdr lis)))))))

; return still changes every time we recur, so we use break instead, which doesn't change
(define multiply-cps
  (lambda (lis return break)
    (cond
      ((null? lis) (return 1))
      ((zero? (car lis)) (break 0))
      (else (multiply-cps (cdr lis) (lambda (v) (return (* (car lis) v))) break)))))

(define multiply
  (lambda (lis break)
    (cond
      ((null? lis) 1)
      ((zero? (car lis)) (break 0))
      (else (* (car lis) (multiply (cdr lis) break))))))

(define goodmult
  (lambda (lis)
    (call/cc
     (lambda (break)
       (multiply lis break)))))

; given a list and atom, returns the 0-indexed index of the first occurrence, -1 if not found
(define indexof
  (lambda (x lis)
    (call/cc
     (indexof-helper x lis (lambda (v) v)))))

(define indexof-helper
  (lambda (x lis break)
       (cond
         ((null? lis) (break -1))
         ((eq? (car lis) x) 0)
         (else (+ 1 (indexof x (cdr lis)))))))

(define mycont (lambda (v) v))

(define replaceall
  (lambda (x y lis)
    (call/cc ; call/cc doesn't do anything cause it returns at the lambda anything, just creating a break point at beginning of each stack frame
     (lambda (k)
       (cond
         ((null? lis) (k '()))
         ((eq? x (car lis)) (cons y (replaceall x y (cdr lis))))
         (else (cons (car lis) (replaceall x y (cdr lis)))))))))

(define replaceall
  (lambda (x y lis)
    (call/cc
     (lambda (k)
       (cond
         ((null? lis) (begin (set! mycont k) '()))
         ((eq? x (car lis)) (cons y (replaceall x y (cdr lis))))
         (else (cons (car lis) (replaceall x y (cdr lis)))))))))

(define myappendit
  (lambda (l1 l2)
    (if (null? l1)
        (mycont l2)
        (cons (car l1) (myappendit (cdr l1) l2)))))