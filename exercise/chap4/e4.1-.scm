
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
  (map car (cdr exp)))

(define (let-exps exp)
  (map (cadr (cdr exp))))

(define (let-body exp)
  (cddr exp))

(define (let->combinations exp)
  (cons 
   (make-lambda (let-vars exp) (let-body exp))
   (let-exps exp))
  )

;;; exercise 4.7
(define (let*? exp)
  (tagged-list? exp 'let*))

(define (sequence->let seqs body)
  (if (last-exp? seqs)
      (cons 'let seqs body)
      (cons 'let (list (first-exp seqs))
	    (sequence->let (rest-exp seqs) body))))

(define (let*->nested-let exp)
  (let ((seqs (cadr exp))
	(body (cddr exp)))
    (sequnece-let seqs body)))

;;; exercise 4.8
(define (named-let? exp) 
  (and (let? exp) (not (null? (cdddr exp)))))

(define (named-let-body exp)
  (cdddr exp))

(define (named-let-var exp)
  (cadr exp))

(define (named-let-exps exp)
  (map car (caddr exp)))

(define (named-let-vals exp)
  (map car (caddr exp)))

(define (named-let->combination exp)
  (make-begin
   ((list 'define (named-let-var exp) (make-lambda (named-let-exps exp)
					     (named-let-body exp)))
    (cons (named-let-var exp) (named-let-vals exp)))))

;; rewirte procedure let->combination
(define (let->combination exp)
  (cond ((named-let? exp) (named-let->combination exp))
	((let? exp)   (cons 
		       (make-lambda (let-vars exp) (let-body exp))
		       (let-exps exp)))
	(else (error "not a let expression --- in let->combination"))))

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
  (tagged-list? 'while))

(defien (while-pred exp)
  (cadr exp))
(define (while-body exp)
  (cddr exp))

(define (while->combination exp)
  (make-begin
   (list 'define 'while-fun 
	 (make-lambda '() 
		      (make-if (while-pred exp)
			       (make-begin 
				(while-body exp)
				'(while-fun)))))
   '(while-fun)))

;;; exercise 4.11
(define (make-frame vars vals)
  (cond ((and (null? vars) (null? vals))
	 '())
	((and (not (null? vars)) (not (null? vals)))
	 (cons (cons (car vars) (car vals)) 
	       (make-frame (cdr vars) (cdr vals))))
	(else 
	 (error "vars and vals have diffterent lengths! --- MAKE_FRAME"))))

(define (frame-variables f)
  (map car f))

(define (frame-values f)
  (map cdr f))

(define (add-binding-to-frame var val f)
  (set! f (cons (cons var val)
		f)))

;;; exercise 4.12
(define (lookup-in-frame var f)
  (define (iter vars vals)
    (if (null? vars)
	'()
	(if (eq? var (car vars))
	    (car vals)
	    (iter (cdr vars) (cdr vals)))))
  (let ((vars (frame-variables f))
	(vals (frame-values f)))
    (iter vars vals)))

(define (has-variable-in-frame? var f)
  (let ((vars (frame-variables f)))
    (memq var vars)))

(define (set-in-frame! var val f)
  ;; this code is for binding-implemetation, see exercise 4.11
  (let ((binding (assq var f)))
    (if binding
	(set-cdr! binding val))))

(define (set-in-frame! var val f)
  ;; this code is for list-implemetation
  (define (iter vars vals)
    (if (eq? var (car vars))
	(set-car! vals val)
	(iter (cdr vars) (cdr vals))))
  (let ((vars (car f))
	(vals (cadr f)))
    (iter vars vals)))

(define (lookup-variable-value var env)
  ((member-procedure (lambda (f)
		       (not (null? f))))
   (map lookup-in-frame env)))

(define (define-variable! var val env)
  (if (has-variable-in-frame (first-frame env))
      (set-in-frame! var val (first-frame env))
      (add-binding-to-frame var val (first-frame env))))

(define (set-variable! var val env)
  ((member-procedure (lambda (f)
		       (if (has-variable-in-frame? var f)
			   (begin
			     (set-in-frame var val env)
			     'true)
			   'false)))
   env))

;;; exercise 4.13
(define (del-from-frame! var f)
  (define (iter vars vals)
    (if (null? vars)
	'()
	(if (eq? var (car vars))
	    (begin (set! vars (cdr vars))
		   (Set! vals (cdr vals))
		   'OK)
	    (iter (cdr vars) (cdr vals)))))
  (iter (frame-variables f) (frame-values f)))

(define (make-unbound! var env)
  (let ((f (first-frame env)))
    (if (null? (del-from-frame! var f))
	(make-unbound! var (enclosing-enviroment env)))))
