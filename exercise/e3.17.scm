;;; exercise 3.17 
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define (my-count-pairs x)
  (define (member-of? item lst)
    (if (null? lst)
        false
        (if (eq? item (car lst))
            true
            (member-of? item (cdr lst)))))

  (define visited (list))

  (define (helper lst)
    (if (and (pair? lst) (not (member-of? lst visited)))
        (begin (set! visited (cons lst visited))
               (+ 1
                  (helper (car lst))
                  (helper (cdr lst))))        
        0))
  
  (helper x))

;; test cases
(define z3 (list 'a 'b 'c))
(count-pairs z3)
(my-count-pairs z3)

(define a (list 'a 'b))
(define z4 (cons a (cdr a)))
(count-pairs z4)
(my-count-pairs z4)

 (define second (cons 'a 'b)) 
 (define third (cons 'a 'b)) 
 (define first (cons second third)) 
 (set-car! third second) 
 (count-pairs first)  ;; => 4 
(my-count-pairs first)

(define a (list 'a))
(define b (cons a a))
(define z7 (cons b b))
(count-pairs z7)
(my-count-pairs z7)

(define a (list 'a 'b 'c))
(set-cdr! (cddr a) a)
(my-count-pairs a)
(count-pairs a)

;;; exercise 3.18

(define (is-cycle? x)
  (define visited (list))
  (define (test lst)
    (if (pair? lst)
        (if (memq lst visited)
            true
            (begin  (set! visited (cons lst visited))
                    (test (cdr lst))))
        false))
  (test x))

;; test cases           
(define a (list 'a 'b 'c))
(set-cdr! (cddr a) a)
(is-cycle? a)


;;; exercise 3.19
(define (is-cycle? x)
  (define (iter lst current)
    (cond ((or (null? lst) (null? current)) false)
	  ((eq? current x) true)
	  (else (iter lst (cdr current)))))
  (iter x (cdr x)))

(define (is-cycle? x)
  (define (test p1 p2)
  ;;; - p1 is the first pair, p2 is the second pair
  ;;; - p1 goes forword one step once while p2 goes forword two steps
  ;;; - if there is a cycle, after some steps, p2 will "catch" p1, that
  ;;;   means p1 equal to p2.
    (cond ((or (null? p1) (null? p2)) false)
	  ((and (pair? p1) (pair? p2))
	   (if (eq? p1 p2)
	       true
	       (if (pair? (cdr p2))
		   (test (cdr p1) (cddr p2))
		   false)))
	  (else false)))
  (if (pair? x)
      (test x (cdr x))
      false))

;; test case    
(define a (list 'a 'b 'c))
(is-cycle? a)
(set-cdr! (cddr a) a)
(is-cycle? a)

(define b (list 1 2 3 4))
(is-cycle? b)
(set-cdr! (cdddr b) (cddr b))
(is-cycle? b)


;;; exercise 3.20
(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x)
