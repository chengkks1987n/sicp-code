#lang scheme

(define nil (list))

;;; exercise 2.32

(define (subsets s)
  (if (null? s) (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest (map 
                      (lambda (set)
                        (cons (car s) set))
                      rest)))))


;(define ls (list 1 2 3))
;(subsets null)
;(subsets (list 1))
;(subsets ls)

;;; exercise 3.33
(define (accumulate  op init lst)
  (cond ((null? lst) init)
        (else (op (car lst)
                  (accumulate op init (cdr lst))))))

;(accumulate + 0 (list 1 2 3 4))
;(accumulate * 1 (list 1 2 3 4))
;(accumulate cons null (list 1 2 3))
;(accumulate list null (list 1 2 3))
;(accumulate append null (list 1 2 3))

(define (my-filter predicate sequence)
  (cond ((null? sequence) null)
        (else (if (predicate (car sequence))
                  (cons (car sequence) (filter predicate (cdr sequence)))
                  ((filter predicate (cdr sequence)))))))
;(my-filter odd? (list 1 2 3 4 5))

(define (my-map p sequence)
  (accumulate (lambda (x y) 
                (cons (p x) y))
              null 
              sequence))
;(define (sq x) (* x x))
;(my-map sq (list 1 2 3))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
;(my-append (list 1 2) (list 3 4))

(define (my-length sequence)
  (accumulate (lambda (element result)
                (cond ((null? element) result)
                      (else (+ 1 result))))
              0
              sequence))
;(my-length (list 1 2 3 4 5))

;;; exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
;(horner-eval 2 (list 1 3 0 5 0 1))

(define (enumate-leaves t)
  (cond ((null? t) null)
        ((not (pair? t)) (list t))
        (else (append (enumate-leaves (car t))
                      (enumate-leaves (cdr t))))))

;; exercise 2.35
(define (count-leaves t)
  (accumulate (lambda (elem rlt) (+ rlt (length elem)))
              0
              (map enumate-leaves t)))

;(define x (cons (list 1 2) (list 3 4)))
;(define lst (list x x))
;(enumate-leaves x)
;(enumate-leaves lst)
;(count-leaves x)
;(count-leaves lst)

;;; exercise 2.36
(define (first-column seqs)
  (map (lambda (x) (car x)) seqs)) ;; same as (map car seqs) !!!!!!
(define (rest-column seqs)
  (map (lambda (x) (cdr x)) seqs))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (first-column seqs))
            (accumulate-n op init (rest-column seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
;(first-column s)
;(rest-column s)
;(accumulate-n + 0 s)

;;; exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))

(define v1 (list 1 2 3 4))
(define v2 (list 4 5 6 6))
(define v3 (list 6 7 8 9))
(define m1 (list v1 v2 v3))

;(define m2 (list v1 v2))
;(define m3 (transpose m2))
;(dot-product v1 v1)
;(dot-product v1 v2)
;(dot-product v1 v3)
;(matrix-*-vector m1 v1)
;(transpose m1)
;(matrix-*-matrix m1 m3)

;;; exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

;(fold-right / 1 (list 1 2 3))
;(fold-left / 1 (list 1 2 3))
;(fold-right list null (list 1 2 3))
;(fold-left list null (list 1 2 3))
;;if (op a b) is the same as (op b a), 
;;means that we can change ther order of the operand
;;then the result of fold-right and fold -left will be same.
;;commutative 

;; take the matrix multiplication into acount, you will find the upper answer is wrong,
;; op must to be associative !!


;;; exercise 2.39
(define (reverse1 sequence)
  (fold-right (lambda (element result) (append result (list element))) nil sequence))
(define (reverse2 sequence)
  (fold-left (lambda (result element) (cons element result)) nil sequence))

;(define lst (list 1 2 3 4))
;(reverse1 lst)
;(reverse2 lst)

;;; exercise 2.40
(define (flatmap op seq)
  (accumulate append null (map op seq)))

(define (enumerate-interval start end step)
  (cond ((> start end) null)
        (else (cons start (enumerate-interval (+ start step) end step)))))
;(enumerate-interval 1 10 2)
;(enumerate-interval 1 10 1)

(define (unique-pairs n)
  (flatmap (lambda (x)
             (map (lambda (y) (list x y)) 
                  (enumerate-interval 1 (- x 1) 1)))             
           (enumerate-interval 1 n 1)))
;(unique-pairs 4)

(define (prime? n)
  (define (div? n m)
  (= 0 (remainder n m)))
  (define up (sqrt n))
  (define (iter k)
    (cond ((> k up) true)
          ((div? n k) false)
          (else (iter (+ 1 k)))))
  (iter 2))
;(prime? 1)
;(prime? 2)
;(prime? 3)
;(prime? 4)
;(prime? 5)
;(prime? 6)
;(prime? 7)
(define (sum-prime? lst)
  (prime? (+ (car lst) (cadr lst))))
;(sum-prime? (list 1 2))
;(sum-prime? (list 7 2))
(define (prime-sum-pair n)
  (filter sum-prime? (unique-pairs n)))
;(prime-sum-pair 6)
 
;;; exercise 2.41
(define (unique-triple n)
  (flatmap (lambda (x)
             (map (lambda (y) (cons x y))
                  (unique-pairs (- x 1))))
           (enumerate-interval 1 n 1)))
;(unique-triple 6)
(define (triple-sum n s)
  (filter (lambda (lst) (= s (accumulate + 0 lst)))
          (unique-triple n)))
;(triple-sum 6 10)
