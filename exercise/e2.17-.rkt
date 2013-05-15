#lang scheme

;;; exercise 2.17
(define (last-pair lst)
  (cond ((null? lst) null)
        ((null? (cdr lst)) (car lst))
        (else (last-pair (rest lst)))))
;(last-pair (list 1 4 9 16 25))
;(last-pair (list 23 72 149 34))

;;; exercise 2.18
  (define (butlast lst)
    (cond ((null? lst) null)
          ((null? (cdr lst)) null)
          (else (cons (first lst) (butlast (rest lst))))))
 ;(butlast (list 1 4 9 16 25))
 ;(butlast (list 23 72 149 34))
  
(define (reverse lst)
  (cond ((null? lst) null)
        (else (cons (last-pair lst) (reverse (butlast lst))))))
;(reverse (list 1 4 9 16 25))
;(reverse (list 23 72 149 34))

;;; exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
 
(define (no-more? values) (null? values))
(define (first-denomination values) (first values))
(define (except-first-denomination values) (rest values))

(define (cc amount coin-values)
   (cond ((= amount 0) 1)
         ((or (< amount 0) (no-more? coin-values)) 0)
         (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))
;(cc 100 us-coins)
;(cc 100 (reverse us-coins))
        

;;; excercise 2.20
(define (same-parity x . y)
  (define (find x lst)
    (cond ((empty? lst) (list x))
          ((even? (+ x (first lst))) (cons x (find (first lst) (rest lst))))
          (else (find x (rest lst)))))
  (find x y))
;(same-parity 1 2 3 4 5 6 7)
;(same-parity 2 3 4 5 6 7)


;;; exercise 2.21
(define square (lambda (n) (* n n)))
(define (square-list items)
   (if (null? items)
       null
       (cons (square (first items)) (square-list (rest items)))))
(define (square-list2 items) (map square items))
 ;(square-list (list 1 2 3 4))
 ;(square-list2 (list 1 2 3 4))

;;; exercise 2.22
;;a
;;b answer is a list ,its end is null
;;b we can use (append answer (list (square (car things))))

;;; exercise 2.23
(define (foreach f lst)
  (cond ((not (null? lst)) 
         (f (first lst)) (foreach f (rest lst))))) 
;(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
;(foreach (lambda (x) (newline) (display x)) (list 57 321 88))