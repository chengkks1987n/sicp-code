(load "meval.scm")
(load "environment.scm")
(load "syntax.scm")
;;;; exercise 4.20
(define t1 '(define (f x)
	      (let ((even?
		     (lambda (n)
		       (if (= n 0)
			   true
			   (odd? (- n 1)))))
		    (odd?
		     (lambda (n)
		       (if (= n 0)
			   false
			   (even? (- n 1))))))
		(cons (even? x) (odd? x)))))

(define t2 '(let ((fact
		   (lambda (n)
		     (if (= n 1)
			 1
			 (* n (fact (- n 1)))))))
	      (fact 10)))

;;;;; use let, get errors
(m-eval t1 the-global-environment)
;(m-eval '(f 5) the-global-environment)
; error>
;;Unbound variable -- LOOKUP even?
;(m-eval t2 the-global-environment)
;error> 
;Unbound variable -- LOOKUP fact

;;; a)
(define (letrec? exp)
  (tagged-list? exp 'letrec))
(define letrec-variables let-bound-variables)
(define letrec-values let-values)
(define letrec-body let-body)

(define (letrec->let exp)
  (let ((vars (letrec-variables exp))
	(vals (letrec-values exp))
	(body (letrec-body exp)))
    (let ((bindings (map (lambda (var)
			   (list var ''*unassigned*))
			 vars))
	  (sets (map (lambda (var val)
		       (list 'set! var val))
		     vars vals)))
      (make-let bindings (append sets body)))))

;; test letrec->let
(define tt2 '(letrec ((fact
		    (lambda (n)
		      (if (= n 1)
			  1
			  (* n (fact (- n 1)))))))
	       (fact 10)))
(letrec->let tt2)
;Value 11: (let ((fact (quote *unassigned*))) (set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))) (fact 10))

(define tt1 '(letrec ((even?
		       (lambda (n)
			 (if (= n 0)
			     true
			     (odd? (- n 1)))))
		      (odd?
		       (lambda (n)
			 (if (= n 0)
			     false
			     (even? (- n 1))))))
	       (cons (even? 5) (odd? 5))))
(letrec->let tt1)
;Value 12: (let ((even? (quote *unassigned*)) (odd? (quote *unassigned*))) (set! even? (lambda (n) (if (= n 0) true (odd? (- n 1))))) (set! odd? (lambda (n) (if (= n 0) false (even? (- n 1))))) (cons (even? 5) (odd? 5)))

;; install letrec to m-eval
(define (m-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))    
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
	((let? exp) (m-eval (let->application exp) env))
	((letrec? exp) (m-eval (letrec->let exp) env))  ; install letrec
        ((application? exp)
         (m-apply (m-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

;; test letrec expressions
(m-eval tt1 the-global-environment)
;Value 14: (#f . #t)
(m-eval tt2 the-global-environment)
;Value: 3628800

(define ttt1 '(define (f x)
		(letrec ((even?
			  (lambda (n)
			    (if (= n 0)
				true
				(odd? (- n 1)))))
			 (odd?
			  (lambda (n)
			    (if (= n 0)
				false
				(even? (- n 1))))))
		  (cons (even? x) (odd? x)))))
(m-eval ttt1 the-global-environment)
(m-eval '(f 5) the-global-environment)
;Value 16: (#f . #t)

;;; b)
;; 1. If you use internal definitions, the the environment will not change.
;;    the new defined variables are in the same environment.
;; 2. If you use let, the original environment will be extended, a new frame
;;    will be created, the variables in let will create in the new frame.
;;    the values of the variables are evaluated in the original environment.
;; 3. If you use letrec, the original environment will be extended, a new frame
;;    will be created, the variables in let will create in the new frame. but
;;    different to let, the values of the variables are evaluated in the new frame.

;;;;; exercise 4.21
;; run it in scheme
((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
	  1
	  (* k (ft ft (- k 1)))))))
 10)
;Value: 3628800

;; run it in m-eval
(define t1 '((lambda (n)
	       ((lambda (fact)
		  (fact fact n))
		(lambda (ft k)
		  (if (= k 1)
		      1
		      (* k (ft ft (- k 1)))))))
	     10))
(m-eval t1 the-global-environment)
;Value: 3628800

;; a)
; run in scheme
(define fib (lambda (n)
	      ((lambda (f1)
		 (f1 f1 n))
	       (lambda (f2 k)
		 (if (< k 2)
		     k
		     (+ (f2 f2 (- k 1)) (f2 f2 (- k 2))))))))
(fib 6)
;Value: 8

; run in m-eval
(m-eval '((lambda (n)
	    ((lambda (f1)
	       (f1 f1 n))
	     (lambda (f2 k)
	       (if (< k 2)
		   k
		   (+ (f2 f2 (- k 1)) (f2 f2 (- k 2)))))))
	  6)
	the-global-environment)
;Value: 8

;;; b)
(define (f x)
  (define (even? n)
    (if (= n 0)
	true
	(odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
	false
	(even? (- n 1))))
  (even? x))
(f 5)
;Value: #f

; my verson of no internal definitions
(define ff (lambda (x)
	     ; you can take f1 as evven?, f2 as odd?
	     ((lambda (f1 f2)
		(f1 f2 f1 x))
	      (lambda (f2 f1 k) ; like even?
		(if (= k 0)
		    true
		    (f2 f1 f2 (- k 1))))
	      (lambda (f1 f2 k) ; linek odd?
		(if (= k 0)
		    false
		    (f1 f2 f1 (- k 1)))))))
(ff 5)
;Value: #f
(ff 6)
;Value: #t

; this version is from the book
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
(f 5)
;Value: #f
(f 6)
;Value: #t
