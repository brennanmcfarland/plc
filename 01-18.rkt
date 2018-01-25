; return the length of a list
(define len
	(lambda (lis)
		(if (null? lis)
			0
			(+ 1 (len (cdr lis))))))

; returns true if the element a is in the list lis
(define member?
	(lambda (a lis)
		(if (null? lis)
			#f
			(if (eq? a (car lis))
				#t
				(member? a (cdr lis))))))
; member? using cond
(define member?
	(lambda (a lis)
		(cond
			((null? lis) #f)
			((eq? a (car lis)) #t)
			(else (member? a (cdr lis))))))
; factorial x
(define factorial
	(lambda (x)
		(if (zero? x)
			1
			(* x (factorial (- x 1))))))
; count the number of numbers in a list
(define countnums
	(lambda (lis)
		(cond
			((null? lis) 0)
			((number? (car lis)) (+ 1 (countnums (cdr lis))))
			(else (+ 0 (countnums (cdr lis)))))))
; sum the numbers in a list
(define sumnumbers
	(lambda (lis)
		(cond
			((null? lis) 0)
			((number? (car lis)) (+ (car lis) (sumnumbers (cdr lis))))
			(else (sumnumbers (cdr lis))))))

