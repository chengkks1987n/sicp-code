(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

;; test cases
(define nil '())
(define left (make-tree 3 nil nil))
(define right (make-tree 2 nil nil))
(define t1 (make-tree 1 left right))

(tree->list-1 left)
(tree->list-1 t1)
(tree->list-2 left)
(tree->list-2 t1)


;;; exercise 2.63
;;  the result is same
;;  ref http://wiki.drewhess.com/wiki/SICP_exercise_2.63


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

;; test cse
(define lst (list 1 3 5 7 9 11))
(define tre (list->tree lst))

;;; exercise 2.65

(define (union-list s1 s2)
  (cond ((or (null? s1) (null? s2)) (append s1 s2))
	((= (car s1) (car s2)) 
	 (cons (car s1) (union-set (cdr s1) (cdr s2))))
	((> (car s1) (car s2))
	 (cons (car s2) (union-set s1 (cdr s2))))
	((< (car s1) (car s2)) 
	 (cons (car s1) (union-set (cdr s1) s2)))))

(define (intersection-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1
		     (intersection-list (cdr set1)
				       (cdr set2))))
	      ((< x1 x2)
	       (intersection-list (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-list set1 (cdr set2)))))))


(define (union-set s1 s2)
  (let ((lst1 (tree->list-2 s1))
	(lst2 (tree->list-2 s2)))
    (let ((lst (union-list lst1 lst2)))
      (list->tree lst))))

(define (intersection-set s1 s2)
  (let ((lst1 (tree->list-2 s1))
	(lst2 (tree->list-2 s2)))
    (let ((lst (intersection-list lst1 lst2)))
      (list->tree lst))))

