
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

;;excercise 2.57
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

;;excercise 2.57
(define (multiplicand s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '* (cddr s))))


(define (exponentiation? e)
  (and (pair? e) (eq? '** (car e))))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponent b e)
  (cond ((=number? e 0) 1)
	((=number? e 1) b)
	;((and (number? b) (number e)) (pow b e))
	(else (list '** b e))))

;; -----------------


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
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponent (base exp) 
                                      (make-sum (exponent exp) -1))))
                                      
	(else (error "unknown expression type -- DERIV" exp))))

;; test

(define t '(+ x y x))

(addend t)

(augend t)

(deriv '(* x y (+ x y 3)) 'x)
