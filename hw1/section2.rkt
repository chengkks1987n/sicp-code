#lang scheme

(define (square n) (* n n))
 ;; Section 2: Recursion
 ;; Question 1
(define (squares nums)
  (if (empty? nums)
      null
      (append (list (square (first nums))) (squares (rest nums)))))
;test
;(squares (list 2 3 4 5))
;(squares (list 7))
;(squares null)
 
 ;; Question 2
(define (switch sent)
  (error "Not Implemented!") ;; Your code goes here
 )
 
 ;; Question 3a
(define (first-streak lst)
  (define (iter lst mark ans)
    (cond ((empty? lst) ans)
          ((null? mark) (iter (rest lst) (first lst) (+ ans 1)))
          ((= mark (first lst)) (iter (rest lst) mark (+ ans 1)))
          (else ans)))
  (iter lst null 0))

(define (first-streak-recur lst)
  (if (empty? lst) 0
      (let ((r (rest lst)))
        (cond ((empty? r) 1)
              ((= (first lst) (first r)) (+ (first-streak-recur r) 1))
              (else 1)))))
         
;test
;(first-streak (list 0))
;(first-streak (list 1))
;(first-streak (list 1 1 0 0 0))
;(first-streak (list 1 0))
;(first-streak (list 0 1))
;(first-streak (list 1 1))
;(first-streak (list 0 0))
;(first-streak (list ))
;(first-streak '(1 0 1 0 0 0))

;(first-streak-recur (list 0))
;(first-streak-recur (list 1))
;(first-streak-recur (list 1 1 0 0 0))
;(first-streak-recur (list 1 0))
;(first-streak-recur (list 0 1))
;(first-streak-recur (list 1 1))
;(first-streak-recur (list 0 0))
;(first-streak-recur '(1 0 1 0 0 0))
;(first-streak-recur (list ))


 ;; Question 3b
(define (best-streak lst)
  (if (empty? lst)
      0
      (max (first-streak lst) (best-streak (rest lst)))))
;test
;(best-streak '(1 0 1 0 0 0))
;(best-streak (list 1 1 0 0 0))
 
