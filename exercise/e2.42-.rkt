#lang racket

;;; exercise 2.42 

(define (accumulate  op init lst)
  (cond ((null? lst) init)
        (else (op (car lst)
                  (accumulate op init (cdr lst))))))

(define (flatmap op seq)
  (accumulate append null (map op seq)))

(define (enumerate-interval a b)
  (if (> a b) 
      null
      (cons a (enumerate-interval (+ a 1) b))))

(define empty-board (list))

(define (safe? k positions)
  (define (single-safe? a i b j)
    (cond ((= a b) false)
          ((= (abs (- a b)) (abs (- j i))) false)
          (else true)))
  (define (col-safe? a i j seq)
    (cond ((null? seq) true)
          ((single-safe? a i (car seq) j) (col-safe? a i (- j 1) (cdr seq)))
          (else false)))
  (if (null? positions) 
      true
      (col-safe? (car positions) k (- k 1) (cdr positions))))

(define (adjoin-position new-row k rest)
  (cons new-row rest))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))