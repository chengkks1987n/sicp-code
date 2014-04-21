(load "meval.scm")
(load "environment.scm")
(load "syntax.scm")

(define (l-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))    
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
			 (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (l-eval (cond->if exp) env))
	((let? exp) (l-eval (let->application exp) env))
        ((application? exp)
         (l-apply (actual-value (operator exp) env)
		  (operands exp)
		  env))
        (else (error "Unknown expression type -- L-EVAL" exp))))

(define (l-apply proc args env)
  (cond ((primitive-procedure? proc)
	 (apply-primitive-procedure 
	  proc
	  (list-of-arg-values args env)))
	((compound-procedure? proc)
	 (eval-sequence
	  (procedure-body proc)
	  (extend-environment
	   (procedure-parameters proc)
	   (list-of-delayed-args args env)
	   (procedure-environment proc))))
	(else (error "Unknow procedure type --- l-apply" proc))))

(define m-eval l-eval)

(define (actual-value exp env)
  (force-it (l-eval exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
	    (list-of-arg-values (rest-operands exps)
				env))))
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
	    (list-of-delayed-args (rest-operands exps)
				  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (l-eval (if-consequent exp) env)
      (l-eval (if-alternative exp) env)))

;;;;representing thunks
(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))
(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
	 (let ((result (actual-value
			(thunk-exp obj)
			(thunk-env obj))))
	   (set-car! obj 'evaluated-thunk)
	   (set-car! (cdr obj) result) ; replace exp with its value
	   (set-cdr! (cdr obj) '()) ; forget unneeded env
	   result))
	((evaluated-thunk? obj)
	 (thunk-value obj))
	(else obj)))

;;; test l-eval
(define t1 '(define (try a b)
	      (if (= a 0) 1 b)))
(define t2 '(try 0 (/ 1 0)))
(l-eval t1 the-global-environment)
(l-eval t2 the-global-environment)
;Value: 1


