(define (rand-update x)
  (remainder (+ 29 (* 17 x)) 31))

;;; exercise 3.81
(define rand-init 23)
(define generator-rand
  (cons-stream rand-init (stream-map rand-update generator-rand)))

(stream-head generator-rand 10)
;Value: (23 17 8 10 13 2 1 15 5 21)

(define (reset-rand init)
  (define a (cons-stream init (stream-map rand-update a)))
  a)
(stream-head (reset-rand rand-init) 10)
;Value: (23 17 8 10 13 2 1 15 5 21)
(stream-head (reset-rand 20) 10)
;Value: (20 28 9 27 23 17 8 10 13 2)

;;; exercise 3.81 referance:http://community.schemewiki.org/?sicp-ex-3.81
(define (random-number-generator command-stream) 
  (define random-number 
    (cons-stream rand-init 
		 (stream-map (lambda (number command)  
			       (cond ((null? command) the-empty-stream) 
				     ((eq? command 'generator) 
				      (rand-update number)) 
				     ((and (pair? command)  
					   (eq? (car command) 'reset)) 
				      (cadr command)) 
				     (else  
				      (error "bad command -- " commmand)))) 
			     random-number 
			     command-stream))) 
  random-number)

(stream-head (random-number-generator 
	      (stream 'generator '(reset 17) 'generator))
	     4)
;Value: (23 17 17 8)

;;; exercise 3.82
(define (random-in-range low high)
  (+ low (random (- high low))))
(random-in-range 1.0 2.0)
;Value: 1.2458126465382098
(random-in-range 1.0 2.0)
;Value: 1.6717131673227261

(define (point-in-rect x1 x2 y1 y2)
  (cons (random-in-range x1 x2)
	(random-in-range y1 y2)))
(define (rect-area x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))

(define (stream-points-in-rect x1 x2 y1 y2)
  (cons-stream (point-in-rect x1 x2 y1 y2)
	       (stream-points-in-rect x1 x2 y1 y2)))

(define (stream-estimate-integral p? x1 x2 y1 y2)
  (define p (stream-map p? (stream-points-in-rect x1 x2 y1 y2)))
  (define t (cons-stream '(0 0)
			 (stream-map (lambda (p1 t1)
				       (if p1
					   (list (+ 1 (car t1))
						 (+ 1 (cadr t1)))
					   (list (car t1)
						 (+ 1 (cadr t1)))))
				     p t)))
  (stream-map (lambda (x) 
		(* (rect-area x1 x2 y1 y2)
		   (/ (car x) (cadr x)))) 
	      (stream-cdr t)))
(define (pred? p)
  (> (car p) (cdr p)))
(define ans (stream-estimate-integral pred? 0.0 1.0 0.0 1.0))
(stream-head ans 12)
;Value: (0 .5 .3333333333333333 .5 .4 .3333333333333333 .42857142857142855 .5 .4444444444444444 .4 .36363636363636365 .3333333333333333)
(stream-ref ans 100)
;Value: .5742574257425742
(stream-ref ans 10000)
;Value: .5003499650034996
