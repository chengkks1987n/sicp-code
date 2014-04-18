(load "meval.scm")
(load "environment.scm")
(load "syntax.scm")
;;; exercise 4.14

;;; primivate-procedure map 
;;; see in file environment.scm
;(m-eval '(pmap car '((a 1) (b 2) (c 3))) the-global-environment)
; error>
;The object (primitive #[compiled-procedure 11 ("list" #x1) #xf #x321823]) is not applicable.

;(m-eval '(pmap car '((a 1) (b 2) (c 3))) the-global-environment) is converted
; to (apply map '(car '((a 1) (b 2) (c 3)))), the 'car is just a symbol not 
; a primivate-procedure for scheme. it should be (apply map (list car '((a 1) ..

;; because the map is a high order procedure, one of the arguments of map is
; also a procedure!  the procedure argument is applyed to scheme-apply just
;  as symbol, not evaluated by self-define apply, so things go wrong.

;;;  self-define map
(m-eval '(define (my-map pro lst)
	   (if (null? lst)
	       '()
	       (cons (pro (car lst)) (my-map pro (cdr lst)))))
	the-global-environment)
;; test
(m-eval '(my-map car '((a 1) (b 2) (c 3))) the-global-environment)
;Value 16: (a b c)

;;;;; exercise 4.15
; there are only two possible outcomes for the expression (try try) :
; 1. run forever 
;    if the outcome is 'run forever', the result of (halt? p p) must be
;    true, which means (try try) is halted, we get the paradox!
; 2. halted
;    if the outcome is halted, the result of (halt? p p) must be false,
;    which means (try try) is run-forever, we get the paradox!
; after all, we come to the conclution : the procedure halt? is impossible.


;;;; exercise 4.16 a
(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- LOOKUP" var)
      (let* ((frame (first-frame env))
	     (binding (find-in-frame var frame)))
	(if binding
	    (if (eq? '*unassigned* (binding-value binding))
		(warn "unassigned value for variable -- " var)
		(binding-value binding))
	    (lookup-variable-value var (enclosing-environment env))))))

;; test
(m-eval '(define a '*unassigned*) the-global-environment)
;(m-eval 'a the-global-environment)
;; error>
;;unassigned value for variable --  a

;;;;; 4.16 b
(define (scan-out-definitions body)
  (let ((vars '())
	(vals '())
	(others '()))
    (define (scan seqs)
      (cond ((null? seqs) 'done)
	    ((definition? (car seqs))
	     (set! vars (append vars (list (definition-variable (car seqs)))))
	     (set! vals (append vals (list (definition-value (car seqs)))))
	     (scan (cdr seqs)))
	    (else (set! others (append others (list (car seqs))))
		  (scan (cdr seqs)))))
    (scan body)
    (if (not (null? vars))
	(let ((bindings (map (lambda (var)
			       (list var ''*unassigned*))
			     vars))
	      (sets (map (lambda (var val)
			   (list 'set! var val))
			 vars vals)))
	  (list (make-let bindings (append sets others))))
	body)))
      
;; test
(scan-out-definitions '((define (even? n)
			  (if (= 0 n)
			      true
			      (odd? (- n 1))))
			(+ 12 34)
			(define (odd? n)
			  (if (= 0 n)
			      false
			      (even? (- n 1))))
			(cons (even? x) (odd? x))))
;Value 23: ((let ((even? (quote *unassigned*)) (odd? (quote *unassigned*))) (set! even? (lambda (n) (if (= 0 n) true (odd? (- n 1))))) (set! odd? (lambda (n) (if (= 0 n) false (even? (- n 1))))) (+ 12 34) (cons (even? x) (odd? x))))

(scan-out-definitions '((define e 12)
			(define d 11)
			(+ e d)))
;Value 24: ((let ((e (quote *unassigned*)) (d (quote *unassigned*))) (set! e 12) (set! d 11) (+ e d)))

;;; 4.16 c
;install in make-procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-definitions body) env))

;; test
(m-eval '(define (test)
	   (define e 12)
	   (define d 11)
	   (+ e d))
	the-global-environment)
(m-eval '(test) the-global-environment)
;Value: 23

(m-eval '(define (f x)
	   (define (even? n)
	     (if (= 0 n)
		 true
		 (odd? (- n 1))))
	   (define (odd? n)
	     (if (= 0 n)
		 false
		 (even? (- n 1))))
	   (cons (even? x) (odd? x)))
	the-global-environment)
(m-eval '(f 4) the-global-environment)
;Value 28: (#t . #f)

(m-eval '(define (f1 x)
	   (define (even? n)
	     (if (= 0 n)
		 true
		 (odd? (- n 1))))
	   (cons (even? x) (odd? x)) ;this line will move to the end by scan-out-definitions
	   (define (odd? n)
	     (if (= 0 n)
		 false
		 (even? (- n 1)))))
	the-global-environment)
(m-eval '(f1 4) the-global-environment)
;Value 29: (#t . #f)


;;;; exercise 4.17
;;; CODE 1
;(lambda <vars>
;  (define u <e1>)
;  (define v <e2>)
;  <e3>)
;;; CODE 2
;(lambda <vars>
;  (let ((u '*unassigned*)
;	(v '*unassigned*))
;    (set! u <e1>)
;    (set! v <e2>)
;    <e3>))
;;; CODE 3
;(lambda <vars>
;  ((lambda (u v)
;     (set! u <e1>)
;     (set! v <e2>)
;     <e3>)
;   '*unassigned* '*unassigned*))
;;; CODE 4
;(lambda <vars>
;  (define u '*unassigned*)
;  (define v '*unassigned*)
;  (set! u <e1>)
;  (set! v <e2>)
;  <e3>)

; 1. for sequence definitions (C0de-1), only one frame is needed.
; 2. for simultaneous definitions (Code-2 , will reduce to code-3) need two frames,
;    one is for the inner-lambda
; 3. if transform code-1 to code-4, we get the simultaneous definitions and only
;     one frame is needed.

;;;; exercise 4.18
; I think both the two ways (one way is like this, the other is in exercixe 4.16)
;   will work. and the result is the same.

;;;; exercise 4.19
;; simultaneous-definitions, implememted in exercise 4.16
(define t '(let ((a 1))
	     (define (f x)
	       (define b (+ a x))
	       (define a 5)
	       (+ a b))
	     (f 10)))
;(m-eval t the-global-environment)
;Warning: unassigned value for variable --  a

;; sequence-definitions
(load "meval.scm")
(load "environment.scm")
(load "syntax.scm")
(m-eval t the-global-environment)
;Value: 16

;; 1. I think the sequence-definitions is better
;; 2. Ben and Alyssa are both right
;; 3. Eva's strategy is too hard to implement, we need to scan out the internal 
;;    difinitions which dont depend on other definitions. and run them firstly.
;;    if there is no definitions which donnt depend on others, there will be 
;;    another problem.



