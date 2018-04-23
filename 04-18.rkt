; higher order functions

(define foldleft
  (lambda (f i l)
    (cond
      ((null? l) i)
      ;((pair? l) (cons (f i) '()))
      (else (foldleft f (f i (car l)) (cdr l))))))

(define foldright
  (lambda (f i l)
    (if (null? l)
        i
        (f (car l) (foldright f i (cdr l))))))

; map function
(define mapit
  (lambda (f l)
    (if (null? l)
        l
        (cons (f (car l)) (mapit f (cdr l))))))

; flip function
(define flipit
  (lambda (f)
    (lambda (x y)
      (f y x))))

; filter function
(define filterit
  (lambda (f l)
    (cond
      ((null? l) '())
      ((f (car l)) (cons (car l) (filteit f (cdr l))))
      (else (filterit f (cdr l))))))

; apply function: (applyit * '(2 3 4) '(10 20 30)) -> (20 60 120)
(define applyit
  (lambda (f a b)
    (if (null? a)
        '()
        (cons (f (car a) (car b)) (applyit f (cdr a) (cdr b))))))

; myreverse, implemented with the higher order functions
(define myreverse
  (lambda (lis)
    (foldleft (flipit cons) '() lis)))