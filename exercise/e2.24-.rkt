#lang scheme

;;;exercise 2.24

;;;exercise 2.25
(define lst1 '(1 3 (5 7) 9))
(define lst2 '((7)))
(define lst3 '(1 (2 (3 (4 (5 (6 7)))))))
(car (cdr (car (cdr (cdr lst1)))))
(car (car lst2))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr lst3))))))))))))

;;;exercise 2.26  
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)

;;;exercise 2.27
(define (reverse x)
  (cond ((null? x) null)
        (else (append (reverse (cdr x)) (list (car x))))))
(define (deep-reverse x)
  (cond ((null? x) null)
        ((not (pair? x)) x)
        (else (append (deep-reverse (cdr x))
                      (list (deep-reverse (car x)))))))
(define lst (list (list 1 2) (list 3 4)))
(deep-reverse 1)
(deep-reverse (list 1))
(deep-reverse (list 1 2))
(deep-reverse lst)
  
;;;exercise 2.28
(define (fringe tree)
  (cond ((null? tree) tree) 
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))
(fringe (list 1))
(fringe (list 1 2))
(fringe lst)


  