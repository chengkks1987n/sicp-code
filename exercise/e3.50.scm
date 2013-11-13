(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

(define the-empty-stream '())

(define (stream-null? s) (null? s))

;; memo-proc, (void -> T) - > (void -> T)
;; take a procedure with no argument as input,
;; return a procedur with no argument.
;; the first time you call the returned procedure,
;; it call the input procedur and return its result, also memory
;; its result.
;; next time you call it, just return the result.
;; 
;; it is effective.
;;
;; proc, void -> T, a procedure with no argument 
(define (memo-proc proc)
  (let ((already-run false) (result false))
    (lambda ()
      (if (not already-run)
	  (begin (set! result (proc))
		 (display "now running")
		 (newline)
		 (set! already-run true)
		 result)
	  result))))

;; test for memo-proc
(define (t-proc) 
  (display "test procedue - no retrun value")
  (newline))

(define (n-proc) 
  (display "another test procedue - have retrun value")
  (newline)
  'done)

(define m-t (memo-proc t-proc))
(define m-n (memo-proc n-proc))
(m-n)
;=> another test procedue - have retrun value
;=> now running
;=> ;Value: done

(m-n)
;=> ;Value: done
(m-t)
;=> test procedue - no retrun value
;=> now running
;=> ;Unspecified return value

(m-t)
;=> ;Unspecified return value
;;------------------------------

;;cons-stream is buildin in mit-scheme. see http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Streams.html
;(define cons-stream cons)  

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))


;;; exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-cdr argstreams))))))

;;; exercise 3.51
(define (stream-enumerate-interval a b)
  (if (> a b)
      the-empty-stream
      (cons a (delay (stream-enumerate-interval (+ 1 a) b)))))


(define x (stream-map show (stream-enumerate-interval 0 10)))
;=> 
;0
;Value: x
(stream-ref x 5)
;=>
;1
;2
;3
;4
;5
;;va;ue: 5
(stream-ref x 7)
;=>
;6
;7
;;value: 7

;;; exercise 3.52

(define (display-stream s)
  (stream-for-each display-line s))

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; sum = 1
(define y (stream-filter even? seq))
; sum = 6
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
			 seq))
; sum = 10
(stream-ref y 7) ;; the index starts from 0!
; sum = 136
(display-stream z)
; sum = 210
;]=>
;10
;15
;45
;55
;105
;120
;190
;210
;;Unspecified return value

;;; exercise 3.53
(define (add-stream s1 s2)
  (cons-stream (+ (stream-car s1) (stream-car s2))
	       (add-stream (stream-cdr s1) (stream-cdr s2))))

(define s (cons-stream 1 (add-stream s s)))
; 1 2 4 8 16 32 64 128 256 ......

;test
(define (show upper)
  (define (iter n)
    (if (< n upper)
	(begin (display (stream-ref s n))
	       (display " ")
	       (iter (+ 1 n)))))
  (iter 0))
(show 10)
;1 ]=> 1 2 4 8 16 32 64 128 256 512 

;;; exercise 3.54
(define (mul-stream s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))
(stream-head ones 20)
;Value : (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)

(define integers (cons-stream 1 (add-stream ones integers)))
(stream-head integers 20)
;Value : (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)

(define factorials (cons-stream 1 (mul-stream factorials (stream-cdr integers))))
(stream-head factorials 10)
;Value : (1 2 6 24 120 720 5040 40320 362880 3628800)

;;; exercise 3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
	       (add-stream (stream-cdr s) (partial-sums s))))
(define k (partial-sums integers))
(stream-head k 10)
;Value : (1 3 6 10 15 21 28 36 45 55)

;;; exercise 3.56
(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else (let ((c1 (stream-car s1))
		    (c2 (stream-car s2)))
		(cond ((< c1 c2) 
		       (cons-stream c1 (merge (stream-cdr s1) s2)))
		      ((< c2 c1)
		       (cons-stream c2 (merge (stream-cdr s2) s1)))
		      (else 
		       (cons-stream c1 (merge (stream-cdr s1) 
					      (stream-cdr s2)))))))))

(define s (cons-stream 1 (merge (merge (scale-stream s 2)
				       (scale-stream s 3))
				(scale-stream s 5))))
(stream-head s 20)
;Value : (1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36)

;;; exercise 3.57


