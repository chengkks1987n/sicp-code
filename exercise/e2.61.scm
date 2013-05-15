;;; exercise 2.61

(define (adjoin-set x s1)
  (cond ((null? s1) (list x))
	((= x (car s1)) s1)
	((< x (car s1)) (cons x s1))
	((> x (car s1)) (cons (car s1) (adjoin-set x (cdr s1))))
	(else (error "in adjoin-set" x s1))))

;; test cases
(define s1 (list 4 7 19))
(adjoin-set 4 s1)
(adjoin-set 7 s1)
(adjoin-set 19 s1)
(adjoin-set 2 s1)
(adjoin-set 10 s1)
(adjoin-set 21 s1)

;;; exercise 2.62

(define (union-set s1 s2)
  (cond ((or (null? s1) (null? s2)) (append s1 s2))
	((= (car s1) (car s2)) 
	 (cons (car s1) (union-set (cdr s1) (cdr s2))))
	((> (car s1) (car s2))
	 (cons (car s2) (union-set s1 (cdr s2))))
	((< (car s1) (car s2)) 
	 (cons (car s1) (union-set (cdr s1) s2)))))

;; test case
(define s2 (list 3 7 12 24))
(union-set s1 s2)
(union-set s2 s1)

  
	    

