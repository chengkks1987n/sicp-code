
(define (equal? x y)
  (cond ((and (null? x) (null? y)) true)
        ((and (not (pair? x)) (not (pair? y))) (eq? x y))
        ((and (pair? x) (pair? y)
              (equal? (car x) (car y))
              (equal? (cdr x) (cdr y)))
         true)
        (else false)))

(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
	(else (union-set (cdr s1) (adjoin-set (car s1) s2)))))

(define s1 '(a b c))
(define s2 '(b c d))

(element-of-set? 'a s1)
(element-of-set? 'a s2)
(adjoin-set 'a s1)
(adjoin-set 'a s2)
(intersection-set s1 s2)
(union-set s1 s2)

;;; execise 2.60, use duplicate representation
;;  element-of-set?  O(n)
;;  adjoin-set  O(1)
;;  intersection-set O(n*n)
;;  union-set O(n)
;; user the non-duplicate one is better
;; the length of set is much shorter than the duplicate one.
