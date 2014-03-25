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
