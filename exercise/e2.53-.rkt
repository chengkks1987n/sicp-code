#lang racket

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;;; exercise 2.54

(define (equal? x y)
  (cond ((and (null? x) (null? y)) true)
        ((and (not (pair? x)) (not (pair? y))) (eq? x y))
        ((and (pair? x) (pair? y)
              (equal? (car x) (car y))
              (equal? (cdr x) (cdr y)))
         true)
        (else false)))

  (equal? '(this is a list) '(this is a list))
  (equal? '(this is a list) '(this (is a) list))
  (equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6)) 
  (equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5 7) 6))

;;; exercese 2.55
;; the follow three expressions are same.
(car ''abcd)
(car (quote (qutoe abcd)))
(car '(qutoe abcd))
