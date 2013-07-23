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


