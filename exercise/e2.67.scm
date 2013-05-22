;; leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; huffman tree
(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; decode
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; others
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)    ; symbol
			       (cadr pair))  ; frequency
		    (make-leaf-set (cdr pairs))))))

;;; excercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

;;; excercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))
(define (element-of-set? x set)
  (cond ((null? set) false)
	((eq? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

;; my encode-symbol

(define (encode-symbol sym tree)
  (define (encode-helper s t)
    (if (leaf? t) (list)
	(let ((left (left-branch t)))
	  (if (element-of-set? s (symbols left))
	      (cons 0 (encode-helper s left))
	      (cons 1 (encode-helper s (right-branch t)))))))
  (if (element-of-set? sym (symbols tree))
      (encode-helper sym tree)
      (error "symbol is not in the tree")))

;; test cases
(encode-symbol 'a sample-tree)
(encode-symbol 'd sample-tree)
(encode-symbol 'c sample-tree)
(encode-symbol 'f sample-tree)
(encode '(a d a b b c a) sample-tree)
(encode '(a d a b b c a f) sample-tree)

;;; execercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
;; my successive-merge
(define (successive-merge leafs)
  (cond ((null? leafs) (error "leafs can not be null"))
	((= 1 (length leafs)) (car leafs))
	(else (successive-merge 
	       (adjoin-set (make-code-tree (car leafs)
					   (cadr leafs))
			   (cddr leafs))))))

;; test cases
(define sample-pairs '((a 4) (b 2) (c 1) (d 1)))
(define my-tree (generate-huffman-tree sample-pairs))

(decode sample-message my-tree)
(encode-symbol 'a my-tree)
(encode-symbol 'd my-tree)
(encode-symbol 'c my-tree)
(encode-symbol 'f my-tree)
(encode '(a d a b b c a) my-tree)
(encode '(a d a b b c a f) my-tree)

;;; excercise 2.70
(define e2-70-pairs '((A 2) (NA 16) (BOOM 1) (SHA 3) 
		      (GET 2) (YIP 9) (JOB 2) (WAH 1)))
(define e2-70-syms '(Get a job
			 Sha na na na na na na na na
			 Get a job
			 Sha na na na na na na na na
			 Wah yip yip yip yip yip yip yip yip yip
			 Sha boom))

(define e2-70-huffman-tree (generate-huffman-tree e2-70-pairs))

(define e2-70-code (encode e2-70-syms e2-70-huffman-tree))

(length e2-70-code)

(* (length e2-70-syms) 3)









