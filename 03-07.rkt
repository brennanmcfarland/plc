; these are answers to the homework problems, may need to use this in interpreter

(define merge-cps
  (lambda (lis1 lis2 return)
    (cond
      ((null? lis1) (return lis2))
      ((null? lis2) (return lis1))
      ((< (car lis1) (car lis2)) (merge-cps (cdr lis1) lis2 (lambda (v) (return (cons (car lis1) v)))))
      (else (merge-cps lis1 (cdr lis2) (lambda (v) (return (cons (car lis2) v))))))))

(define dup*-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '()))
      ((pair? (car lis)) (dup*-cps (car lis) (lambda (v1) (dup*-cps (cdr lis) (lambda (v2) (return (cons v1 (cons v1 v2))))))))
      (else (dup*-cps (cdr lis) (lambda (v) (return (cons (car lis) (cons (car lis) v)))))))))

(define removedups*-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return lis))
      ((null? (cdr lis)) (return lis))
      ((pair? (car lis)) (removedups*-cps (car lis) (lambda (v1) (removedups*-cps (cdr lis) (lambda (v2) (return (cons v1 v2)))))))
      ((eq? (car lis) (cadr lis)) (removedups*-cps (cdr lis) return)) ; not modifying anything, so can just pass return
      (else (removedups*-cps (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

(define split-cps
 (lambda (lis return)
   (cond
     ((null? lis) (return '() '()))
     ((null? (cdr lis)) (return lis '()))
     (else (split-cps (cddr lis) (lambda (v1 v2) (return (cons (car lis) v1) (cons (cadr lis) v2))))))))

(define mergesort-cps
  (lambda (lis return)
    (if (or (null? lis) (null? (cdr lis)))
        (return lis)
        (split-cps lis (lambda (l1 l2) (mergesort-cps l1 (lambda (s1) (mergesort-cps l2 (lambda (s2) (merge-cps s1 s2 return))))))))))

(define replaceatoms-cps
  (lambda (lis1 lis2 return)
    (cond
      ((null? lis1) (return lis1 lis2))
      ((null? lis2) (return lis1 lis2))
      ((pair? (car lis1)) (replaceatoms-cps (car lis1) lis2 (lambda (v1 r1) (replaceatoms-cps (cdr lis1) r1 (lambda (v2 r2) (return (cons v1 v2) r2))))))
      (else (replaceatoms-cps (cdr lis1) (cdr lis2) (lambda (v r) (return (cons (car lis2) v) r)))))))

(define suffix
  (lambda (lis)
    (call/cc ; putting it here means that when it breaks to it it breaks to the very bottom of the stack
     (lambda (break)
       (suffix-with-break lis break)))))

(define suffix-with-break
  (lambda (lis break)
    (cond
      ((null? lis) '())
      ((eq? 'x (car lis)) (break (suffix-with-break (cdr lis) break)))
      (else (cons (car lis) (suffix-with-break (cdr lis) break))))))

(define empty-sublists
  (lambda (lis)
    (call/cc
     (lambda (break)
       (empty-sublists-with-break lis break)))))

(define empty-sublists-with-break
  (lambda (lis break)
    (cond
      ((null? lis) '())
      ((eq? 'x (car lis)) (break '()))
      ((list? (car lis)) (cons (call/cc (lambda (break2) (empty-sublists-with-break (car lis) break2))) (empty-sublists-with-break (cdr lis) break)))
      (else (cons (car lis) (empty-sublists-with-break (cdr lis) break))))))