#lang scheme

;;;exercise 2.30

(define (sq x) (* x x))

(define (sq-tree t)
  (cond  ((null? t) null)
         ((not (pair? t)) (sq t))
         (else (cons (sq-tree (car t))
                    (sq-tree (cdr t))))))

(define (square-tree t)
  (map (lambda (st)
         (cond ((pair? st) (square-tree st))
               (else (sq st))))
       t))

(define lst  (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))
(square-tree lst)

(sq-tree lst)

;;; exercise 2.31

(define (tree-map prc tree)
  (map (lambda (sub-tree)
         (cond ((pair? sub-tree) (tree-map prc sub-tree))
               (else (prc sub-tree))))
       tree))

(define (tree-map2 prc tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (prc tree))
        (else (cons (tree-map2 prc (car tree))
                    (tree-map2 prc (cdr tree))))))

(define (sq1-tree tree)
  (tree-map sq tree))

(define (sq2-tree tree)
  (tree-map2 sq tree))

(sq1-tree lst)
(sq2-tree lst)
