
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


;;(define (sum? x)
;;  (and (pair? x) (eq? (cadr x) '+) (not (null? (cddr x)))))

(define (remove-paren x)
  (if (and (pair? x) (null? (cdr x)))
      (remove-paren (car x))
      x))
		    

(define (sum? x)
  (if (and (pair? x) (not (null? (cdr x))))
      (if (eq? (cadr x) '+)
	  true
	  (sum? (cddr x)))
      false))

(define (addend exp)
  (define (my-help x)
    (if (eq? (cadr x) '+)
	(car x)
	(append (list (car x) (cadr x)) (list (addend (cddr x))))))
  (remove-paren (my-help exp)))

(define (augend exp)
  (define (my-help x)
    (if (eq? (cadr x) '+)
	(cddr x)
	(my-help (cddr x))))
  (remove-paren (my-help exp)))

;;(define (addend s) (car s))

;;(define (augend s) (caddr s))

(define (product? x)
  (not (sum? x)))

(define (multiplier exp)
  (define (my-help p) (car p))
  (remove-paren (my-help exp)))

(define (multiplicand exp)
  (define (my-help s)
    (if (null? (cdddr s))
	(caddr s)
	(cddr s)))
  (remove-paren (my-help exp)))


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


(define exp '(x + 3 * (x + (y + 2))))
(define x1 '(a * b * c + d))
(define x2 '(a * b + c))
(define x3 '(a + b * c))
(sum? exp)
(sum? x1)
(sum? x2)
(sum? x3)
(addend exp)
(addend x1)
(addend x2)
(addend x3)
(augend exp)
(augend x2)
(augend x1)
(augend x3)

(deriv exp 'x)
(deriv x1 'a)
(deriv x2 'a)
(deriv x3 'a)
