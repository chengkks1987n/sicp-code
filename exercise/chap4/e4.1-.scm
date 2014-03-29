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
  (let ((consequent true)
	(alternative false))
    (if (not (last-exp? exps))
	(set! consequent 
	      (and->if (rest-exp exps))))
    (make-if (first-exp exps) consequent alternative)))

;; derived or
(define (or->if exps env)
  (let ((consequent true)
	(alternative false))
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
