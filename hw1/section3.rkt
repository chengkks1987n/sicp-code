#lang racket

(define (square n) (* n n))
(define (inc n) (+ n 1))
 ;; Section 3: Higher Order Procedures
 ;; Question 1:
 (define (my-every fn sent)
   (if (empty? sent)
       null
       (append (list (fn (first sent))) (my-every fn (rest sent)))))
 ;test
 ;(my-every square '(1 2 3 4))
 ;(my-every first (list (list 1 2) (list 4 7)))
 
 
 ;; Question 2:
 (define (repeated fn ntimes)
   (lambda (x)
     (if (= ntimes 0)
         x
         ((repeated fn (- ntimes 1)) (fn x)))))
 ;test
 ;((repeated inc 7) 0)
 ;((repeated rest 2) '(the rain in spain))       
 
 ;; Question 3a:
 (define (insert new-num sorted-sent)
   (if (empty? sorted-sent)
       (list new-num)
       (if (<= (first sorted-sent) new-num) 
           (append (list (first sorted-sent)) 
                   (insert new-num (rest sorted-sent)))
           (append (list new-num) sorted-sent))))
 ;(insert 3 '(1 2 3 4 5))
 
 ;; Question 3b:
 (define (insertion-sort unsorted-sent)
	(define (insertion-sort-helper unsorted-sent sorted-sent)
          (if (empty? unsorted-sent) sorted-sent
              (insertion-sort-helper (rest unsorted-sent) 
                                     (insert (first unsorted-sent) sorted-sent)))
	)
    (insertion-sort-helper unsorted-sent '())
  )
  ;(insertion-sort '(3 1 4 1 5))
 
 ;; Question 3c:
 (define (odd-first num1 num2)
   (cond ((and (even? num1) (even? num2)) (> num1 num2))
         ((and (even? num1) (odd? num2)) #t)
         ((and (odd? num1) (even? num2)) #f)
         (else (> num1 num2))))
 
 (define (hof-insert comparator new-num sorted-sent)
    (if (empty? sorted-sent)
       (list new-num)
       (if (comparator (first sorted-sent) new-num)
           (append (list new-num) sorted-sent)
           (append (list (first sorted-sent)) 
                   (hof-insert comparator new-num (rest sorted-sent))))))
 ;(hof-insert odd-first 2 '(1))
 ;(hof-insert odd-first 2 '(1 3))
 ;(hof-insert odd-first 2 '(1 1 3 5 2 4))
 
 (define (hof-insertion-sort comparator sent)
  	(define (insertion-sort-helper unsorted-sent sorted-sent)
          (if (empty? unsorted-sent) sorted-sent
              (insertion-sort-helper (rest unsorted-sent) 
                                     (hof-insert comparator
                                      (first unsorted-sent) sorted-sent))))
    (insertion-sort-helper sent null)
   )
 ;(hof-insertion-sort  odd-first '(3 1 4 1 5 2))
