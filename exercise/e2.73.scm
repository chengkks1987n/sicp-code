;;; exercise 2.73b
;;

(define (install-my-deriv)
  (define (deriv-sum operands)
    '())
  (define (deriv-product operands)
    '())
  (put '+ 'deriv deriv-sum)
  (put '* 'deriv deriv-product))

;; exercise 2.73c
(put '** 'deriv (lambda (operands)
		  '()))

;;; exercise 2.74
(put <division> <op> <item>)
(get <division> <op>)

(put 'IT-division 'get-record (lambda (key)
		    <read IT division records>))

(define (get-record division name)
  ((get division 'get-record) name))

;; b
(define (get-salary division name)
  ((get division 'get-salary) name))

;; c
(define (find-employee name divisions)
  (cond ((null? divisions) false)
	((not (get-record (car divisions) name))
	 (find-employee name (cdr divisions)))
	(else (get-record (car divisions) name))))
