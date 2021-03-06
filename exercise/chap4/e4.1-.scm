;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; in this chapter, the evaluator used to test my codes is from the project5.

(load "../../project/project5/meval.scm")
(load "../../project/project5/environment.scm")
(load "../../project/project5/syntax.scm")
(define eval m-eval)
;;;; exercise 4.1
;; eval from left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
	(cons first (list-of-values (rest-operands exps) env)))))

;; tests
(list-of-values '((begin (display 'left) 'left)
		  (begin (display 'right) 'right))
		the-global-environment)
; ]=> leftright
;;Value 11: (left right)

;; eval from right to left

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
	(cons (eval (first-operand exps) env) rest))))

;; tests
(list-of-values '((begin (display 'left) 'left)
		  (begin (display 'right) 'right))
		the-global-environment)
; ]=> rightleft
;Value 12: (left right)

;;;; exercise 4.2 (a)
;; when apply procedure 'application?' on the 'cond clauses', true will be returned
;; (define x 3) will be regarded as a procedure.

;;;; exercise 4.2 (b)
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
;; tests
(eval '(call + 1 2) the-global-environment)
;Value: 3


(load "../../project/project5/meval.scm")
(load "../../project/project5/environment.scm")
(load "../../project/project5/syntax.scm")
;;; exercise 4.3 
(define eval-dispath-data '())
(define (put-eval! sym proc)
  (set! eval-dispath-data (cons (list sym proc) eval-dispath-data)))
(define (get-eval sym)
  (let ((p (assq sym eval-dispath-data)))
    (if p (cadr p) #f)))
; get the tag of expression
(define (exp-tag exp) (car exp))
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((pair? exp)
	 (let ((proc (get-eval (exp-tag exp))))
	   (if proc
	       (apply proc (list exp env))
	       (if (application? exp)
		   (m-apply (eval (operator exp) env)
			    (list-of-values (operands exp) env))
		   (error "unknown")))))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(eval '+ the-global-environment)
;Value 35: (primitive #[arity-dispatched-procedure 19])
(eval '12 the-global-environment)
;Value: 12
(eval "abc" the-global-environment)
;Value 36: "abc"
(eval '(+ 2 3) the-global-environment)
;Value: 5
;(eval '(s 2 3) the-global-environment)
;error!=> Unbound variable -- LOOKUP s

;; add quote
(put-eval! 'quote (lambda (exp env)
		    (text-of-quotation exp)))
(eval ''s the-global-environment)
;Value: s
(eval ''(s b) the-global-environment)
;Value 41: (s b)

;; add some more types of expressions such as if, cond, let, deffine...
;; and test them....

;;; exercise 4.4
;; special form of and
(define (and? exp)
  (tagged-list? exp 'and))

(define (and-exps exp) (cdr exp))

(define (eval-and exps env)
  (if (null? exps) true
      (if (last-exp? exps)
	  (eval (first-exp exps) env)
	  (if (eval (first-exp exps) env)
	      (eval-and (rest-exps exps) env)
	      false))))
;; test and
(put-eval! 'and (lambda (exp env)
		  (eval-and (and-exps exp) env)))
(eval '(and) the-global-environment)
;Value: #t
(eval '(and 12) the-global-environment)
;Value: 12
(eval '(and true 12) the-global-environment)
;Value: 12
(eval '(and true false) the-global-environment)
;;Value: #f
(eval '(and #f #f #f) the-global-environment)
;;Value: #f
(eval '(and #t #t #t) the-global-environment)
;Value: #t
(put-eval! 'begin (lambda (exp env)
		    (eval-sequence (begin-actions exp) env)))
(eval '(and (begin (display '1) #t)
	    (begin (display '2) #f)
	    (begin (display '3) #f))
      the-global-environment)
; ]=> 12
;Value: #f

;; special form of or
(define (or? exp)
  (tagged-list? exp 'or))

(define (or-exps exp) (cdr exp))
(define (eval-or exps env)
  (if (null? exps) true
      (if (last-exp? exps)
	  (eval (first-exp exps) env)
	  (if (eval (first-exp exps) env)
	      true
	      (eval-or (rest-exps exps) env)))))
;; test or
(put-eval! 'or (lambda (exp env)
		  (eval-or (or-exps exp) env)))
(eval '(or) the-global-environment)
;Value: #t
(eval '(or 12) the-global-environment)
;Value: 12
(eval '(or false 2) the-global-environment)
;Value: 2
(eval '(or true false) the-global-environment)
;Value: #t
(eval '(or #f #f #f) the-global-environment)
;Value: #f
(eval '(or (begin (display '1) #f)
	   (begin (display '2) #t)
	   (begin (display '3) #f))
      the-global-environment)
; ]=> 12
;Value: #t

;; derived and  
(define (and->if exps)
  (if (null? exps) 'true
      (if (last-exp? exps)
	  (first-exp exps)
	  (make-if (first-exp exps) 
		   (and->if (rest-exps exps))
		   'false))))

;;test derived and
(put-eval! 'and (lambda (exp env)
		  (eval (and->if (and-exps exp)) env)))
(put-eval! 'if eval-if)
(eval '(and) the-global-environment)
;Value: #t
(eval '(and 2) the-global-environment)
;Value: 2
(eval '(and true 12) the-global-environment)
;Value: 12
(eval '(and true false) the-global-environment)
;;Value: #f
(eval '(and #f #f #f) the-global-environment)
;;Value: #f
(eval '(and #t #t #t) the-global-environment)
;Value: #t
(eval '(and (begin (display '1) #t)
	    (begin (display '2) #f)
	    (begin (display '3) #f))
      the-global-environment)
; ]=> 12
;Value: #f

;; derived or
(define (or->if exps)
  (if (null? exps) 'true
      (if (last-exp? exps)
	  (first-exp exps)
	  (make-if (first-exp exps) 
		   'true
		   (or->if (rest-exps exps))))))

;; test derived or
(put-eval! 'or (lambda (exp env)
		 (eval (or->if (or-exps exp)) env)))
(eval '(or) the-global-environment)
;Value: #t
(eval '(or 12) the-global-environment)
;Value: 12
(eval '(or false 2) the-global-environment)
;Value: 2
(eval '(or true false) the-global-environment)
;Value: #t
(eval '(or #f #f #f) the-global-environment)
;Value: #f
(eval '(or (begin (display '1) #f)
	   (begin (display '2) #t)
	   (begin (display '3) #f)) the-global-environment)
; ]=> 12
;Value: #t

;;; exercise 4.5
(define (special-clause? clause)
  (tagged-list? (cdr clause) '=>))
(define special-test car)
(define special-recipient caddr)
(define (special->normal-clause c)
  (list (special-test c) (list (special-recipient c) (special-test c))))
;; change procedure cond-actions to this:
(define (first-cond-clause clauses)
  (if (special-clause? (car clauses))
      (special->normal-clause (car clauses))
      (car clauses)))

;; test
;; fistly add procedure 'assoc' to primivate procedure
(m-eval '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
	       (else false))
	the-global-environment)
;Value: 2

;;; exercise 4.6
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
;; test
(define e '(let ((a 1)
		 (b 2))
	     (display (+ a b))
	     (display (+ a a b))
	     (* a b)))
(let-vars e)
(let-exps e)
(let-body e)
(let->combinations e)
(put-eval! 'let (lambda (exp env)
		  (eval (let->combinations exp) env)))
(put-eval! 'lambda (lambda (exp env)
		     (make-procedure 
		      (lambda-parameters exp) (lambda-body exp) env)))
(eval e the-global-environment)
; ]=> 34
;Value: 2

;;; exercise 4.7
(define (let*? exp)
  (tagged-list? exp 'let*))

(define (sequence->let seqs body)
  (if (last-exp? seqs)
      (append (list 'let seqs) body)
      (append (list 'let (list (first-exp seqs)))
	      (list (sequence->let (rest-exps seqs) body)))))

(define (let*->nested-let exp)
  (let ((seqs (cadr exp))
	(body (cddr exp)))
    (sequence->let seqs body)))

;; TEST
(DEFINE E '(LET* ((X 3)
		  (Y (+ X 2))
		  (Z (+ X Y 5)))
		 (display y)
		 (* X Z)))
(let*? e)
;Value: #t
(let*->nested-let e)
;Value 67: (let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (display y) (* x z))))
(put-eval! 'let* (lambda (exp env)
		   (eval (let*->nested-let exp) env)))
(eval e the-global-environment)
; ]=> 5
;Value: 39


;;; exercise 4.8
(define (named-let? exp) 
  (and (let? exp) (symbol? (cadr exp))))
(define (named-let-body exp)
  (cdddr exp))
(define (named-let-var exp)
  (cadr exp))
(define (named-let-exps exp)
  (map car (caddr exp)))
(define (named-let-vals exp)
  (map cadr (caddr exp)))
(define (named-let->combination exp)
  (make-begin
   (list
    (list 'define (named-let-var exp) 
	  (make-lambda (named-let-exps exp)
		       (named-let-body exp)))
    (cons (named-let-var exp) (named-let-vals exp)))))

(define (new-let->combination exp)
  (cond ((named-let? exp) (named-let->combination exp))
	((let? exp) (let->combinations exp))
	(else (error "not a let expression --- in let->combination"))))

;; test
(define e '(let ((a 1)
		 (b 2))
	     (display (+ a b))
	     (display (+ a a b))
	     (* a b)))
(define e1 '(define (fib n)
	     (let iter ((a 1)
			(b 0)
			(count n))
	       (if (= count 0)
		   b
		   (iter (+ a b) a (- count 1))))))
(define x '(let iter ((a 1)
		      (b 0)
		      (count 5))
	     (if (= count 0)
		 b
		 (iter (+ a b) a (- count 1)))))
(named-let? e1)
;Value: #f
(named-let? e)
;Value: #f
(named-let? x)
;Value: #t
(named-let->combination x)
;Value 82: (begin (define iter (lambda (a b count) (if (= count 0) b (iter (+ a b) a (- count 1))))) (iter 1 0 5))
(put-eval! 'let (lambda (exp env)
		  (eval (new-let->combination exp) env)))
(eval x the-global-environment)
;Value: 5
(eval e the-global-environment)
; ]=> 34
;Value: 2


;;; exercise 4.9
;; design a new expression 'while'
;(while <prediction>
;       <body>)

;; will switch to 
;(define tmp-fun 
;  (lambda () 
;    (if <prediction>
;	(begin 
;	  <body>
;	  tmp-fun))))
;(tmp-fun)

(define (while? exp)
  (tagged-list? exp 'while))
(define (while-pred exp)
  (cadr exp))
(define (while-body exp)
  (cddr exp))

(define (while->combination exp)
  (make-begin
   (list
    (list 'define 'while-fun 
	  (make-lambda
	   '() 
	   (list (make-if (while-pred exp)
			  (make-begin 
			   (append (while-body exp)
				   '((while-fun))))
			  ''done))))
   '(while-fun))))

;; test
(define we '(while (< i 10)
		   (display i)
		   (set! i (+ i 2))))
(while->combination we)
;Value 60: (begin (define while-fun (lambda () (if (< i 10) (begin (display i) (set! i (+ i 2)) (while-fun)) (quote done)))) (while-fun))
(put-eval! 'while (lambda (exp env)
		    (eval (while->combination exp) env)))
(put-eval! 'define eval-definition)
(put-eval! 'set! eval-assignment)
(define e '(define i 3))
(eval e the-global-environment)
(eval 'i the-global-environment)
;Value: 3
(eval we the-global-environment)
; ]=>3579
;Value: done

(define t '(let ((i 1))
	     (while (< i 10)
		    (display i)
		    (set! i (+ i 2)))))

;; while?  must be added to m-eval
;; because in 't' while is convert to the body of lambda
;; its evaluation will reduce to the use m-eval.

;; that means for t the while is evaluated use m-eval, not eval

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
;	((do-while? exp) (eval-do-while exp env))
	((while? exp) (m-eval (while->combination exp) env))
        ((application? exp)
         (m-apply (m-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))
(eval t the-global-environment)
; ]=> 13579
;Value: done
(m-eval t the-global-environment)
;]=> 13579
;Value: done

