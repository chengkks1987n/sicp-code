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
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-stream (stream-cdr fibs)
					fibs))))

; we perform  n-1 times addtions for the nth Fibonacci number.
; Without hte memo-proc procedure it will be fib(n)+1 times.

;;; exercise 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))
(define x (expand 1 7 10))
; 1 4 2 8 5 7 ....(loop 1 4 2 8 5 7)...
(stream-head x 20)
;Value: (1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4)

(define x (expand 3 8 10))
; 3 7 5 0 ... (loop 0)...
(stream-head x 10)
;Value : (3 7 5 0 0 0 0 0 0 0)

;;; exercise 3.59
(define (integrate-series s)
  (mul-stream s (stream-map (lambda (x) (/ 1 x)) integers)))
;; test case
(define  x (integrate-series integers))
(stream-head x 10)
;Value : (1 1 1 1 1 1 1 1 1 1)
(define  x (integrate-series ones))
(stream-head x 10)
;Value : (1 1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9 1/10)

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(stream-head exp-series 10)
;Value: (1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880)

(define cosine-series
  (cons-stream 1 (stream-map 
		  (lambda (x) (- x))
		  (integrate-series sine-series))))
(define  sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(stream-head cosine-series 20)
;Value: (1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0 -1/3628800 0 1/479001600 0 -1/87178291200 0 1/20922789888000 0 -1/6402373705728000 0)
(stream-head sine-series 20)
;Value: (0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880 0 -1/39916800 0 1/6227020800 0 -1/1307674368000 0 1/355687428096000 0 -1/121645100408832000)

;;; exercise 3.60
(define (mul-series s1 s2)
  (let ((car1 (stream-car s1)) (car2 (stream-car s2))
	(cdr1 (stream-cdr s1)) (cdr2 (stream-cdr s2)))
    (cons-stream (* car1 car2)
		 (add-stream
		  (add-stream (scale-stream cdr1 car2)
			      (scale-stream cdr2 car1))
		  (cons-stream 0 (mul-series cdr1 cdr2))))))
;; there is a clever way:
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-stream (scale-stream (stream-cdr s2) (stream-car s1)) 
			    (mul-series (stream-cdr s1) s2))))

;;test
(define x (add-stream (mul-series sine-series sine-series)
		      (mul-series cosine-series cosine-series)))
(stream-head x 10)
;Value: (1 0 0 0 0 0 0 0 0 0)

;;; exercise 3.61
;; s start with a constant 1.
(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s)
					   (invert-unit-series s))
			       -1)))
;test 1 / (1 - x)
(define zeros (cons-stream 0 zeros))
(define r (invert-unit-series (cons-stream 1 (cons-stream -1 zeros))))
(stream-head r 10)
;; a improved invert-unit-series
;; s start with aconstant c, c cannot be o.
(define (invert-series s)
  (if (= 0 (stream-car s))
      (error "in INVERT-UNIT-SERIESINPUT: cannot be 0")
      (cons-stream (/ 1 (stream-car s))
		   (scale-stream (mul-series (stream-cdr s)
					     (invert-unit-series s))
				 (/ -1 (stream-car s))))))

;; my addtion procedur
;; s is the series of function f(x)
;; scale-series return the series of f(kx)
(define (scale-series s k)
  (cons-stream (stream-car s)
	       (scale-stream (scale-series (stream-cdr s) k)
			     k)))
;test
(define x (scale-series ones 3))
(stream-head x 10)
;Value: (1 3 9 27 81 243 729 2187 6561 19683)
(define x (scale-series ones 2))
(stream-head x 10)
;Value: (1 2 4 8 16 32 64 128 256 512)
(define x (scale-series integers 2))
(stream-head x 10)
;Value: (1 4 12 32 80 192 448 1024 2304 5120)

;test sin(2x)=2sin(x)cos(x)
(define sin2x (scale-series sine-series 2))
(stream-head sin2x 10)
;Value: (0 2 0 -4/3 0 4/15 0 -8/315 0 4/2835)
(define tsx (scale-stream (mul-series sine-series cosine-series) 2))
(stream-head tsx 10)
;Value: (0 2 0 -4/3 0 4/15 0 -8/315 0 4/2835)

;;; exercise 3.62
(define (div-series s1 s2)
  (mul-series s1 (invert-series s2)))

(define tangent-series (div-series sine-series cosine-series))
(stream-head tangent-series 10)

;; you can test sin(2x)/cos(x) equal to 2sin(x) or other things.

;;; exercise 3.63
;<TODO>

;;; exercise 3.64
(define (average  a b)
  (/ (+ a b) 2.0))
(average 2 4)
;Value: 3.

(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(sqrt-improve 1 2)
;Value: 1.5

(define (sqrt-stream x)
  (define g
    (cons-stream 1.0 (stream-map (lambda (gs)
				   (sqrt-improve gs x))
				 g)))
  g)
(stream-head (sqrt-stream 2) 10)
;Value 33: (1. 1.5 1.4166666666666665 1.4142156862745097 1.4142135623746899 1.414213562373095 1.414213562373095 1.414213562373095 1.414213562373095 1.414213562373095)

(define (stream-limit s tolerance)
  (let ((d1 (stream-ref s 0))
	(d2 (stream-ref s 1)))
    (if (< (abs (- d1 d2)) tolerance)
	d2
	(stream-limit (stream-cdr s) tolerance))))
(stream-limit (sqrt-stream 2) 0.01)
;Value: 1.4142156862745097
(stream-limit (sqrt-stream 2) 0.001)
;Value: 1.4142135623746899

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
(sqrt 2 0.01)
;Value: 1.4142156862745097
(sqrt 2 0.001)
;Value: 1.4142135623746899

;;; exercise 3.65
(log 2)
;Value: .6931471805599453

(define (log2-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (log2-summands (+ 1 n)))))
(stream-head (log2-summands 1) 10)
;Value: (1. -.5 .3333333333333333 -.25 .2 -.16666666666666666 .14285714285714285 -.125 .1111111111111111 -.1)

(define log2 (partial-sums (log2-summands 1)))
(stream-head log2 10)
;Value : (1. .5 .8333333333333333 .5833333333333333 .7833333333333332 .6166666666666666 .7595238095238095 .6345238095238095 .7456349206349207 .6456349206349207)
; log 2 is between (.7456349206349207 .6456349206349207)

(define (euler-tansform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s2 (* -2 s1) s0)))
		 (euler-tansform (stream-cdr s)))))

(define euler-log2 (euler-tansform log2))
(stream-head euler-log2 10)
;Value: (.7 .6904761904761905 .6944444444444444 .6924242424242424 .6935897435897436 .6928571428571428 .6933473389355742 .6930033416875522 .6932539682539683 .6930657506744464)
;  log 2 is between (.6932539682539683 .6930657506744464)

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define acc-log2 (accelerated-sequence euler-tansform log2))
(stream-head acc-log2 9)
;Value: (1. .7 .6932773109243697 .6931488693329254 .6931471960735491 .6931471806635636 .6931471805604039 .6931471805599445 .6931471805599427)
; log 2 is between (.6931471805599445 .6931471805599427)

;(stream-head acc-log2 10)
;Invalid floating-point operation


;;; exercise 3.66
; the location of pair (i,j) is:
;  2^i - 1 + 2^(i-1) + (2^i)*(j-i-1) , (j>i);
;  2^i - 1 , (j=i)

; (1,100) -> 198


;;; exercise 3.67
(define (interleave s1 s2)
  (cond ((stream-null? s1) s2)
	(else (cons-stream (stream-car s1) 
			   (interleave s2 (stream-cdr s1))))))

(define x (interleave ones integers))
(stream-head x 20)
;Value: (1 1 1 2 1 3 1 4 1 5 1 6 1 7 1 8 1 9 1 10)

(define (pairs s1 s2)
  (cons-stream (list (stream-car s1) (stream-car s2))
	       (interleave (stream-map 
			    (lambda (x) 
			      (list (stream-car s1) x))
			    (stream-cdr s2))
			   (pairs (stream-cdr s1) (stream-cdr s2)))))
(define x (pairs integers integers))
(stream-head x 20)
;Value: ((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5) (2 4) (1 6) (3 4) (1 7) (2 5) (1 8) (4 4) (1 9) (2 6) (1 10) (3 5) (1 11))

;; produce all pairs of integers(i,j) without the condition i<=j
(define (all-pairs s1 s2)
  (cons-stream (list (stream-car s1) (stream-car s2))
	       (interleave (interleave 
			    (stream-map (lambda (x) 
					  (list (stream-car s1) x))
					(stream-cdr s2))
			    (stream-map (lambda (x)
					  (list x (stream-car s2)))
					(stream-cdr s1)))
			   (pairs (stream-cdr s1) (stream-cdr s2)))))

(define x (all-pairs integers integers))
(stream-head x 20)
;Value: ((1 1) (1 2) (2 2) (2 1) (2 3) (1 3) (3 3) (3 1) (3 2) (1 4) (3 4) (4 1) (2 4) (1 5) (4 4) (5 1) (4 2) (1 6) (4 3) (6 1))


;;; exercise 3.68
; this will not work. it is an infinite loop. 
; in the body of procedure pairs, the second argument for procedure interleave
; will be substituted by procedure pairs, it is a recursion, not delayed.


;;; exercise 3.69

(define (triples s1 s2 s3)
  (define x (stream-map 
	     (lambda (x)
	       (cons (stream-car s1) x))
	     (pairs s2 s3)))
  (cons-stream (stream-car x)
	       (interleave (stream-cdr x)
			   (triples (stream-cdr s1)
				    (stream-cdr s2)
				    (stream-cdr s3)))))
(define x (triples integers integers integers))
(stream-head x 20)
;Value: ((1 1 1) (1 1 2) (2 2 2) (1 2 2) (2 2 3) (1 1 3) (3 3 3) (1 2 3) (2 3 3) (1 1 4) (3 3 4) (1 3 3) (2 2 4) (1 1 5) (4 4 4) (1 2 4) (2 3 4) (1 1 6) (3 4 4) (1 3 4))

(define (pythagorean? x)
  (= (square (caddr x))
     (+ (square (cadr x))
	(square (car x)))))
(pythagorean? (list 1 2 3))
;Value: #f
(pythagorean? (list 3 4 5))
;Value: #t

(define pythagorean (stream-filter pythagorean? x))

(stream-head pythagorean 4)
;Value : ((3 4 5) (6 8 10) (5 12 13) (9 12 15))
