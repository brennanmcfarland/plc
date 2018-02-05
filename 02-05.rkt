; (M_value_int '(+ 3 5)) -> 8
; (M_value_int 6 -> 6
; (M_value_int '(+ (* 2 4) (/ 6 3)) -> 10
; +, -, *, /, %
;(define M_value_int
;  (lambda (in)
;    (cond
;      ((null? in) '())
;      ((number? in) in)
;      ((eq? (car in) '+) (+ (M_value_int (cadr in)) (M_value_int (caddr in))))
;      ((eq? (car in) '-) (- (M_value_int (cadr in)) (M_value_int (caddr in))))
;      ((eq? (car in) '*) (* (M_value_int (cadr in)) (M_value_int (caddr in))))
;      ((eq? (car in) '/) (quotient (M_value_int (cadr in)) (M_value_int (caddr in))))
;      ((eq? (car in) '%) (remainder (M_value_int (cadr in)) (M_value_int (caddr in))))
;      (else (error 'undefined "bad input"))
;      )
;    )
;  )

;above is a bad way to write it; doesn't use abstraction
;below is better:

(define M_value_int
  (lambda (in)
    (cond
      ((null? in) '())
      ((number? in) in)
      ((eq? (operator in) '+) (+ (M_value_int (operand1 in)) (M_value_int (operand2 in))))
      ((eq? (operator in) '-) (- (M_value_int (operand1 in)) (M_value_int (operand2 in))))
      ((eq? (operator in) '*) (* (M_value_int (operand1 in)) (M_value_int (operand2 in))))
      ((eq? (operator in) '/) (quotient (M_value_int (operand1 in)) (M_value_int (operand2 in))))
      ((eq? (operator in) '%) (remainder (M_value_int (operand1 in)) (M_value_int (operand2 in))))
      (else (error 'undefined "bad input"))
      )
    )
  )

;USE HELPER FUNCTIONS like so:

(define operator
  (lambda (e)
    (cadr e)))

(define operand1 car)
(define operand2 caddr)

;it helps with abstractions