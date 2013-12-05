(load "e3.73.scm")
(define add-streams add-stream)

;;; exercise 3.77
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
;Value: 2.716923932235896


(define (integral-V2 delayed-integrand initial-value dt)
  (cons-stream initial-value
	       (let ((integrand (force delayed-integrand)))
		 (if (stream-null? integrand)
		     the-empty-stream
		     (integral (delay (stream-cdr integrand))
			       (+ (* dt (stream-car integrand))
				  initial-value)
			       dt)))))

(define (solve-V2 f y0 dt)
  (define y (integral-V2 (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve-V2 (lambda (y) y) 1 0.001) 1000)
;Value: 2.716923932235896

;;; exercise 3.78
(define (solve-2nd a b y0 dy0 dt)
  (define y (integral-V2 (delay dy) y0 dt))
  (define dy (integral-V2 (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream y b)
			   (scale-stream dy a)))
  y)

;;test 
;; y = exp(t);
(stream-ref (solve-2nd 2 -1 1 1 0.001) 1000)
;Value: 2.716923932235896

;;; exercise 3.79

(define (solve-2nd-version2 f y0 dy0 dt)
  (define y (integral-V2 (delay dy) y0 dt))
  (define dy (integral-V2 (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;; test 
; y =exp(t)
(stream-ref (solve-2nd-version2 
	     (lambda (x1 x2) (/ (+ x1 x2) 2))
	     1 1 0.0001) 10000)
;Value: 2.7181459268252266
(define (combine-stream s1 s2)
  (cons-stream (cons (stream-car s1) (stream-car s2))
	       (combine-stream (stream-cdr s1) (stream-cdr s2))))
;;; exercise 3.80
(define (RLC r l c dt)
  (lambda (i0 vc0)
    (define vc (integral-V2 (delay dvc) vc0 dt))
    (define i (integral-V2 (delay di) i0 dt))
    (define dvc (scale-stream i (/ -1 c)))
    (define di (add-stream (scale-stream vc (/ 1 L))
			   (scale-stream i (/ (- r) l))))
    (combine-stream vc i)))

(define rlc1 (RLC 1 1 0.2 0.1))
(define vc-i (rlc1 0 10))
(stream-head vc-i 10)
;Value 21: ((10 . 0) (10 . 1.) (9.5 . 1.9) (8.55 . 2.66) (7.220000000000001 . 3.249) (5.5955 . 3.6461) (3.77245 . 3.84104) (1.8519299999999999 . 3.834181) (-.0651605000000004 . 3.6359559) (-1.8831384500000004 . 3.2658442599999997))

