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

;;;; 
