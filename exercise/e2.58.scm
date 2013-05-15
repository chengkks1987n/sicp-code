
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+  a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '*  m2))))

(define (sum? x)
;  (breakpoint)
  (and (pair? x) (eq? (cadr x) '+) (not (null? (cddr x)))))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*) (not (null? (cddr x)))))

(define (multiplier p) (car p))

(define (multiplicand s) (caddr s))


(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum (make-product (multiplier exp)
				 (deriv (multiplicand exp) var))
		   (make-product (deriv (multiplier exp) var)
				 (multiplicand exp))))
        ;((exponentiation? exp)
         ;(make-product (exponent exp)
          ;             (make-exponent (base exp) 
           ;                           (make-sum (exponent exp) -1))))
                                      
	(else (error "unknown expression type -- DERIV" exp))))

;; test


(define exp '(x + (3 * (x + (y + 2)))))

(define m '(3 * (x + (y + 2))))
(multiplier m)
(multiplicand m)
(make-sum 
 (make-product (multiplier m)
	      (deriv (multiplicand m) 'x))
 (make-product
	      (deriv (multiplier m) 'x)
	      (multiplicand m)))
(product? m)
(sum? m)
(deriv m 'x)
(deriv exp 'x)
