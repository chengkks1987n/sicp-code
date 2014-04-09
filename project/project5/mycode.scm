(load "meval.scm")
(load "syntax.scm")
(load "environment.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some tests
(m-eval ''s the-global-environment)
;Value: s
(m-eval '12 the-global-environment)
;Value: 12

(m-eval '(+ 1 2) the-global-environment)
;Value: 3
(m-eval '(- 1 2) the-global-environment)
;Value: -1
(m-eval '(/ 1 2) the-global-environment)
;Unbound variable -- LOOKUP /
(m-eval '(* 1 2) the-global-environment)
;Unbound variable -- LOOKUP *

(m-eval '(car '(1 2 3 4)) the-global-environment)
;Value: 1
(m-eval '(cdr '(1 2 3 4)) the-global-environment)
;Value 12: (2 3 4)
(m-eval '(null? '(1 2 3 4)) the-global-environment)
;Value: #f
(m-eval '(null? '()) the-global-environment)
;Value: #t

(m-eval '(if (> 1 2) 'yes 'no) the-global-environment)
;Value: no

(m-eval '(begin (define fib 12)
		(+ fib fib 4))
	the-global-environment)
;Value: 28
(m-eval '(begin (define (fib n)
		  (cond ((= n 0) n)
			((= n 1) n)
			(else (+ (fib (- n 1)) (fib (- n 2))))))
		(fib 4))
	the-global-environment)
;Value: 3

(m-eval '(or 'true 'false) the-global-environment)
;Unbound variable -- LOOKUP or
(m-eval '(and 'true 'false) the-global-environment)
;Unbound variable -- LOOKUP and

(m-eval '(let ((x 10)
	       (y 12))
	   (set! x (+ x x))
	   (+ x y))
	the-global-environment)
;Value: 32

;;;computer exercise 1
(load "environment.scm")
(m-eval '(/ 1 2) the-global-environment)
;Value: 1/2
(m-eval '(* 1 2) the-global-environment)
;Value: 2
(m-eval '(cadr '(1 2 3 4)) the-global-environment)
;Value: 2
(m-eval '(cddr '(1 2 3 4)) the-global-environment)
;Value 13: (3 4)
(m-eval '(begin 
	   (display 'start)
	   (newline)
	   (display 'over))
	the-global-environment)
;start
;over
;;Unspecified return value
