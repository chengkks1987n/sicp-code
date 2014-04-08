;;;; exercise 4.1
;; eval from left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
	(cons first (list-of-values (rest-operands exps) env)))))
;; eval from right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (list-of-values (rest-operands exps) env)))
	(cons (eval (first-operand exps) env) first))))

;;;; exercise 4.2 (a)
;; when apply procedure 'application?' on the 'cond clauses', ithe procedure
;; will return true.

;;;; exercise 4.2 (b)
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

;;; exercise 4.3 
;; get: <key> -> (<exp>, <env> -> ?)
;; put; <key>, (<exp>, <env> -> ?) -> null

(define (exp-tag exp) (car exp))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((pair? exp)
	 (let ((proc (get (exp-tag exp))))
	   (if proc
	       (apply proc exp env)
	       (if (application? exp)
		   (apply (eval (operator exp) env)
			  (list-of-values (operands exp) env))))))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;;; exercise 4.4
;; and
(define (and? exp)
  ;; and-expression must must have at least one prediction
  (and (tagged-list? exp 'and) (not (null? (cdr exp)))))

(define (and-exps exp) (cdr exp))

(define (eval-and exps env)
  (if (last-exp? exp)
      (eval (first-exp exps) env)
      (if (eval (first-exp exp) env)
	  (eval-and (rest-exp exps) env)
	  false)))
;; or
(define (or? exp)
  ;; or-expression must must have at least one prediction
  (and (tagged-list? exp 'or) (not (null? (cdr exp)))))

(define (or-exps exp) (cdr exp))

(define (eval-or exps env)
  (if (last-exp? exp)
      (eval (first-exp exps) env)
      (if (eval (first-exp exp) env)
	  true
	  (eval-or (rest-exp exps) env))))

;;; change procedur 'eval' to this:
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	;;......
	((and? exp) (eval-and (and-exps exp) env))
	((or? exp) (eval-or (or-exps exp) env))
	;;......
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;; derived and  
(define (and->if exps env)
  (let ((consequent 'true)
	(alternative 'false))
    (if (not (last-exp? exps))
	(set! consequent 
	      (and->if (rest-exp exps))))
    (make-if (first-exp exps) consequent alternative)))

;; derived or
(define (or->if exps env)
  (let ((consequent 'true)
	(alternative 'false))
    (if (not (last-exp? exps))
	(set! alternative 
	      (or->if (rest-exp exps))))
    (make-if (first-exp exps) consequent alternative)))

;;; change procedur 'eval' to this:
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	;;......
	((and? exp) (eval-if (and->if (and-exps exp)) env))
	((or? exp) (eval-if (or->if (or-exps exp) env)))
	;;......
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;;; exercise 4.5
;; and this procedure:
(define (special-clause? clause)
  (tagged-list? (cdr clause) '=>))

;; change procedure cond-actions to this:
(define (cond-actions clause)
  (if (special-clause? clause)
      (cons (caddr clause) (cond-predicate clause))
      (cdr clause)))

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