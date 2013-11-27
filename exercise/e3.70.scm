; (pwd) - show current working directory
; (cd "<dir>") - change to <dir>
(load '"e3.50.scm")

;;; exercise 3.70
;; make sure the stream s1 and s2 are sorted by the procedure weight
(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else (let ((w1 (weight (stream-car s1)))
		    (w2 (weight (stream-car s2))))
		(if (<= w1 w2)
		    (cons-stream
		     (stream-car s1)
		     (merge-weighted weight (stream-cdr s1) s2))
		    (cons-stream
		     (stream-car s2)
		     (merge-weighted weight (stream-cdr s2) s1)))))))
;; test merge-weighted
(define tmp (merge-weighted (lambda (x) x) 
			    (stream-map (lambda (x) (* 2 x)) ones)
			    integers))
(stream-head tmp 20)
;Value: (1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)

(define (weighted-pairs weight s1 s2)
  (cons-stream (list (stream-car s1) (stream-car s2))
	       (merge-weighted 
		weight
		(stream-map (lambda (x) (list (stream-car s1) x))
			    (stream-cdr s2))
		(weighted-pairs	weight (stream-cdr s1) (stream-cdr s2)))))

;; test 1
(define (sum-weight p) (apply + p)) ; i + j
(define tmp (weighted-pairs sum-weight integers integers))
(stream-head tmp 20)  
;Value: ((1 1) (1 2) (1 3) (2 2) (2 3) (1 4) (1 5) (2 4) (3 3) (3 4) (2 5) (1 6) (1 7) (2 6) (3 5) (4 4) (4 5) (3 6) (2 7) (1 8))

;; test 2
(define (b-weight p) ; 2i + 3j + 5ij
  (let ((i (car p)) (j (cadr p)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))
(define tmp (weighted-pairs b-weight integers integers))
(stream-head tmp 20)  
;Value: ((1 1) (1 2) (1 3) (2 2) (1 4) (1 5) (2 3) (1 6) (2 4) (1 7) (3 3) (1 8) (2 5) (1 9) (3 4) (2 6) (1 10) (1 11) (2 7) (3 5))

;; test3
(define (div235? x)
  (cond ((= (remainder x 2) 0) false)
	((= (remainder x 3) 0) false)
	((= (remainder x 5) 0) false)
	(else true)))
(div235? 12)
;Value: #f
(div235? 13)
;Value: #t

(define s235 (stream-filter div235? integers))
(stream-head s235 10)
;Value: (1 7 11 13 17 19 23 29 31 37)

(define b-ans (weighted-pairs b-weight s235 s235))
(stream-head b-ans 10)
;Value: ((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23) (1 29) (1 31) (7 7))

;;; exercise 3.71
(define (cube x) (* x x x))
(define (cubic-sum p)
  (+ (cube (car p)) (cube (cadr p))))
(cubic-sum '(2 4))
;Value: 72
(define s (weighted-pairs cubic-sum integers integers))
(stream-head s 20)
;Value: ((1 1) (1 2) (2 2) (1 3) (2 3) (3 3) (1 4) (2 4) (3 4) (1 5) (4 4) (2 5) (3 5) (4 5) (1 6) (2 6) (3 6) (5 5) (4 6) (5 6))
(define (Ramanujan-head n)
  (define (iter last sc count)
    (if (< count 0)
	(list)
	(if (= last (cubic-sum (stream-car sc)))
	    (cons last (iter last (stream-cdr sc) (- count 1)))
	    (iter (cubic-sum (stream-car sc))
		  (stream-cdr sc)
		  count))))
  (iter 0 s n))
(Ramanujan-head 6)
;Value: (1729 4104 13832 20683 32832 39312 40033)

;; use stream paradigm
(define ans 
  (stream-filter (lambda (x) (> x 0))
		 (stream-map  (lambda (x1 x2) 
				(if (= (cubic-sum x1) (cubic-sum x2))
				    (cubic-sum x1)
				    -1))
			      s (stream-cdr s))))
(stream-head ans 5)
;Value: (1729 4104 13832 20683 32832)

;;; exercise 3.72
; use the stream paradigm completely, donnt do it like exercise 3.71
(define (square-sum p)
  (+ (square (car p)) (square (cadr p))))

(square-sum '(2 3))
;Value: 13
(define ss (weighted-pairs square-sum integers integers))
(stream-head ss 20)
;Value: ((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (2 4) (3 4) (1 5) (2 5) (4 4) (3 5) (1 6) (2 6) (4 5) (3 6) (5 5) (1 7) (4 6))

(define ans 
  (stream-filter (lambda (x) (> x 0))
		 (stream-map 
		  (lambda (x1 x2 x3) 
		    (if (and (= (square-sum x1) (square-sum x2))
			     (= (square-sum x1) (square-sum x3)))
			(square-sum x1)
			-1))
		  ss (stream-cdr ss) (stream-cdr (stream-cdr ss)))))
(stream-head ans 5)
;Value: (325 425 650 725 845)
