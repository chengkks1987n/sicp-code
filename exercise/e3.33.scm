;;; exercise 3.33
(define (average a b c)
  (let ((d (make-connector))
	(e (make-connector)))
    (adder a b d)
    (mutiplier c e d)
    (constant 2 e)))


;;; exercise 3.34
;whe you set the value of "b", both of the two "a" does not hava
; a value, 
;so "a" cannt be setted;


;;; exercise  3.35
(define (squarer a b)
  (define (process-new-value) 
    (if (has-value? b)
	(if (< (get-value b) 0)
	    (error "squarer less than 0 -- SQUarer" (get-value b))
	    (set-value! a (sqrt (get-value b)) me))
	(if (has-value? a)
	    (set-value! b (* (get-value a) (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else (error "UNKNOW REQUEST -- in square" request))))
  (connect a me)
  (connect b me)
  me)


;;; exercise 3.37
(define (c* a b)
  (let ((c (make-connector)))
    (mutiplier a b c)
    c))

(define (cv a)
  (let ((b (make-connector)))
    (constant a b)
    b))

(define (c- a b)
  (let ((c (make-connector)))
    (adder c b a)
    c))

(define (c/ a b)
  (let ((c (make-connector)))
    (mutiplier b c a)
    c))


