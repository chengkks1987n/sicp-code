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


