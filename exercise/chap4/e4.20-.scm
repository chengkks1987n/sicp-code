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

;;;; evaluator with syntax analysis
(define (eval exp env)
  ((analyze exp) env))
(define (analyze exp)
  (cond ((self-evaluating? exp)
	 (analyze-self-evaluating exp))
	((quoted? exp) (analyze-quoted exp))
	((variable? exp) (analyze-variable exp))
	((assignment? exp) (analyze-assignment exp))
	((definition? exp) (analyze-definition exp))
	((if? exp) (analyze-if exp))
	((lambda? exp) (analyze-lambda exp))
	((begin? exp) (analyze-sequence (begin-actions exp)))
	((cond? exp) (analyze (cond->if exp)))
	((application? exp) (analyze-application exp))
	(else
	 (error "Unknown expression type -- ANALYZE" exp))))
(define (analyze-self-evaluating exp)
  (lambda (env) exp))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
	(vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
	(vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
	(cproc (analyze (if-consequent exp)))
	(aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
	  (cproc env)
	  (aproc env)))))
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
	(bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
	first-proc
	(loop (sequentially first-proc (car rest-procs))
	      (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
	(error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
	(aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
			   (map (lambda (aproc) (aproc env))
				aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
	 (apply-primitive-procedure proc args))
	((compound-procedure? proc)
	 ((procedure-body proc)
	  (extend-environment (procedure-parameters proc)
			      args
			      (procedure-environment proc))))
	(else
	 (error
	  "Unknown procedure type -- EXECUTE-APPLICATION"
	  proc))))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

;; test for evaluator with syntax analysisi
;;; test self-evaluating
(eval '12 the-global-environment)
;Value: 12
;;; test quote
(eval ''s the-global-environment)
;Value: s
;;; test definition
(eval '(define x 2) the-global-environment)
;Value: ok
;;; test variable
(eval 'x the-global-environment)
;Value: 2
;;; test assignment
(eval '(set! x 3) the-global-environment)
;Value: ok
(eval 'x the-global-environment)
;Value: 3
;;; test if
(eval '(if (> 1 2) 'yes 'no) the-global-environment)
;Value: no
;;; test begin and definitions
(eval '(begin (define fib 12)
	      (+ fib fib 4))
      the-global-environment)
;Value: 28
;;; test begin, cond and application
(eval '(begin (define (fib n)
		(cond ((= n 0) n)
		      ((= n 1) n)
		      (else (+ (fib (- n 1)) (fib (- n 2))))))
	      (fib 4))
      the-global-environment)
;Value: 3

;;;; exercise 4.22
;; these procedures are from exercise 4.6
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-vars exp)
  (map car (cadr exp)))
(define (let-exps exp)
  (map cadr (cadr exp)))
(define (let-body exp)
  (cddr exp))
(define (let->combinations exp)
  (cons 
   (make-lambda (let-vars exp) (let-body exp))
   (let-exps exp))
  )

(define (analyze exp)
  (cond ((self-evaluating? exp)
	 (analyze-self-evaluating exp))
	((quoted? exp) (analyze-quoted exp))
	((variable? exp) (analyze-variable exp))
	((assignment? exp) (analyze-assignment exp))
	((definition? exp) (analyze-definition exp))
	((if? exp) (analyze-if exp))
	((lambda? exp) (analyze-lambda exp))
	((begin? exp) (analyze-sequence (begin-actions exp)))
	((cond? exp) (analyze (cond->if exp)))
	((let? exp) (analyze (let->combinations exp))) ;analyze let
	((application? exp) (analyze-application exp))
	(else
	 (error "Unknown expression type -- ANALYZE" exp))))
;; test let
(eval '(let ((x 10)
	     (y 12))
	 (set! x (+ x x))
	 (+ x y))
      the-global-environment)
;Value: 32

;;;; exercise 4.23
;The version in the book, use the procedure sequencially to combine the
;expressions's analysis results into one procedure.and return this
; procedure 

; the result of Alyssa's verson contains all the expressions's analysis
; result. in eval they are executed sequencially.

;If there is only one expression in sequence, they are the same;
;if there are more than one, the difference will show up

;I cannt tell which one is better.

;;;; exercise 4.24
; this procedure is from project 3 in file 'search.scm'
(define (timed f . args)
  (let ((start (runtime)))
    (let ((val (apply f args)))
      (newline)
      (display "time expended: ")
      (display (- (runtime) start))
      val)))

(define t '(define (fib n)
	     (if (< n 2)
		 n
		 (+ (fib (- n 1)) (fib (- n 2))))))
(timed m-eval t the-global-environment)
(timed m-eval '(fib 20) the-global-environment)
;time expended: 3.5800000000000125
;;Value: 6765
(timed eval t the-global-environment)
(timed eval '(fib 20) the-global-environment)
;time expended: 1.1599999999999966
;Value: 6765

;; From the results,we can see the evalutor with syntax analysis is
;; faster.
;; you can do some more tests.


;;; exercise 4.25
;it will work in normal-order language, but not in applicative-order

; in applicative-order, it will not work.
; to evaluate the factorial, the unless must evaluate, to evaluate uless,
; its second argument factorial must evaluate, this will loop forever.
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))
(define (factorial n)
  (unless (= n 1)
	  (* n (factorial (- n 1)))
	  1))
;(factorial 1)
;Aborting!: maximum recursion depth exceeded

;;; exercise 4.26
(load "meval.scm")
(load "environment.scm")
(load "syntax.scm")
(define (unless? exp)
  (tagged-list? exp 'unless))
(define unless-cond cadr)
(define unless-usual caddr)
(define unless-exception cadddr)
;; private form for unless
(define (eval-unless exp env)
  (if (m-eval (unless-cond exp) env)
      (m-eval (unless-exception exp) env)
      (m-eval (unless-usual exp) env)))
;; derived form for unlesss
(define (unless->cond exp)
  (list 'cond
	(list (unless-cond exp) (unless-exception exp))
	(list 'else (unless-usual exp))))

(define t1 '(unless (= b 0)
		    (/ a b)
		    (begin (display "exception: returning 0")
			   0)))
(m-eval '(define b 0) the-global-environment)
(eval-unless t1 the-global-environment)
; ]=> exception: returning 0
;Value: 0
(m-eval '(define b 1) the-global-environment)
(m-eval '(define a 1) the-global-environment)
(eval-unless t1 the-global-environment)
;Value: 1

(unless->cond t1)
;Value 11: (cond ((= b 0) (begin (display "exception: returning 0") 0)) (else (/ a b)))
(m-eval (unless->cond t1) the-global-environment)
;Value: 1
(m-eval '(define b 0) the-global-environment)
(eval-unless t1 the-global-environment)
; ]=> exception: returning 0
;Value: 0

;Alyssa is right too, if unless is a syntax, we cannt use it as
; procedure arguments and return values of procedures.
; if unless is a procedure, we can wirte code like this:
; (list + - * / unless)
; With unless as a procedure, give us more flexibility

;;;; exercise 4.27
(load "leval.scm")
(l-eval '(define count 0) the-global-environment)
(l-eval '(define (id x)
	   (set! count (+ count 1))
	   x)
	the-global-environment)
(l-eval '(define w (id (id 10))) the-global-environment)
(actual-value 'count the-global-environment)
;Value: 1
(l-eval 'count the-global-environment)
;Value: 1
(actual-value 'w the-global-environment)
;Value: 10
;(l-eval 'w the-global-environment) => this will get thunk of w.
(actual-value 'count the-global-environment)
;Value: 2
(l-eval 'count the-global-environment)
;Value: 2
