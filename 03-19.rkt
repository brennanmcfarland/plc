; sort a list of numbers
(define insertionsort
  (lambda (lis)
    (if (null? lis)
        '()
        (insert (car lis) (insertionsort (cdr lis))))))

; to make it a coroutine, first need to convert to cps
(define insertionsort-cps
  (lambda (lis return)
    (if (null? lis)
        (return '())
        (insertionsort-cps (cdr lis) (lambda (v) (insert-cps (car lis) v return))))))

(define insert
  (lambda (a lis)
    (cond
      ((null? lis) (cons a lis))
      ((< a (car lis)) (cons a lis))
      (else (cons (car lis) (insert a (cdr lis)))))))

(define insert-cps
  (lambda (a lis return)
    (cond
      ((null? lis) (return (cons a lis)))
      ((< a (car lis)) (return (cons a lis)))
      (else (insert-cps a (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; generate pseudo-random numbers (this is a simple way to generate pseudo-random numbers, in common use until the 90s)
(define randomlist
  (lambda (k n a b seed) ; a and b should be really big prime numbers
    (cond
      ((zero? k) '())
      (else (cons (modulo seed n) (randomlist (- k 1) n a b (modulo (+ a seed) b)))))))

(define randomlist-cps
  (lambda (k n a b seed return)
    (cond
      ((zero? k) (return '()))
      (else (randomlist-cps (- k 1) n a b (modulo (+ a seed) b) (lambda (v) (return (cons (modulo seed n) v))))))))

; coroutine versions
(define insertionsort-co
  (lambda (k lis return yield)
    (cond
      ((zero? k) (return '()))
      ((null? lis) (yield 10 (lambda (l y) (insertionsort-co k l return y)))) ; the lambda is called when insertion sort resumes
      ; 10 is the number of elements it sorts at a time before yielding
      (else (insertionsort-co (- k 1) (cdr lis) (lambda (v) (insert-cps (car lis) v return)) yield)))))

(define randomlist-co
  (lambda (k n a b seed return yield)
    (cond
      ((zero? k) (yield (return '()) (lambda (x y) (randomlist-co x n a b seed (lambda (v) v) y)))) ; the outer lambda is called when random list resumes
                 ; the return is the data we pass to the coroutine
      (else (randomlist-co (- k 1) n a b (modulo (+ a seed) b) (lambda (v) (return (cons (modulo seed n) v))) yield)))))
