(load "e3.70.scm")

;;; exercise 3.73
(define (integral x init dt)
  (define ans (cons-stream init
			   (add-stream (scale-stream x dt)
				       ans)))
  ans)
(Define tmp (integral ones 0 1))
(stream-head tmp 20)
;Value: (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)

(define (RC r c dt)
  (lambda (i v0)
    (add-stream 
     (scale-stream i r)
     (scale-stream (integral i v0 dt) (/ 1 c)))))

(define rc1 (RC 5 1 0.5))
(define v (rc1 ones 0))
(stream-head v 20)
;Value: (5 5.5 6. 6.5 7. 7.5 8. 8.5 9. 9.5 10. 10.5 11. 11.5 12. 12.5 13. 13.5 14. 14.5)

;;; exercise 3.74
(define (sign-change-detector d1 d2)
  (cond ((and (>= d1 0) (< d2 0)) -1)
	((and (< d1 0) (>= d2 0)) 1)
	(else 0)))
(sign-change-detector 1 -2)
;Value: -1
(sign-change-detector 1 2)
;Value: 0
(sign-change-detector -1 2)
;Value: 1
(sign-change-detector -1 -2)
;Value: 0

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define sinx (stream-map sin integers))
(stream-head sinx 10)
;Value: (.8414709848078965 .9092974268256817 .1411200080598672 -.7568024953079282 -.9589242746631385 -.27941549819892586 .6569865987187891 .9893582466233818 .4121184852417566 -.5440211108893698)

(define zero-crossings (make-zero-crossings sinx 0))
(stream-head zero-crossings 10)
;Value: (0 0 0 1 0 0 -1 0 0 1)

(define (make-zero-crossings-version2 data)
    (stream-map sign-change-detector data (cons-stream 0 data)))
(define zero-crossings-version2
  (make-zero-crossings-version2 sinx))
(stream-head zero-crossings-version2 10)
;Value: (0 0 0 1 0 0 -1 0 0 1)

;;; exercise 3.75
(define (make-smooth-zero-crossings input-stream last-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avg)
                 (make-zero-crossings (stream-cdr input-stream)
				      (stream-car input-stream)
                                      avpt))))
(define smooth-zero-crossings
  (make-smooth-zero-crossings sinx 0 0))
(stream-head smooth-zero-crossings 10)
;Value: (0 0 0 1 0 0 -1 0 0 1)

;;; exercise 3.76
(define (smooth data)
  (stream-map average data (cons-stream 0 data)))
(define smooth-zero-crossings-version2
  (make-zero-crossings-version2 (smooth sinx)))
(stream-head smooth-zero-crossings-version2 10)

