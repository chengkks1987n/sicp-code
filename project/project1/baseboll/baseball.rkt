#lang racket
(require racket/help)

(provide position)
(provide root1)
(provide root2)
(provide time-to-impact)
(provide travel-distance-simple)
(provide find-best-angle)
(provide travel-distance)
(provide best-angle-for-distance)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gravity 9.8)
(define pi 3.1415926)

(define square
  (lambda (x) (* x x)))

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;;; problem 1
(define position 
  (lambda (a v u t)
  (+ (/ (* a t t) 2.0)
     (* v t)
     u)))

;;; problem 2
(define (root1 a b c)
  (let ((tmp (- (* b b) (* 4.0 a c))))
    (if (< tmp 0)
        null
        (/ (- (- b) (sqrt tmp)) (* 2.0 a)))))

(define (root2 a b c)
  (let ((tmp (- (* b b) (* 4.0 a c))))
    (if (< tmp 0)
        (error "No answer!! for:" a b c)
        (/ (+ (- b) (sqrt tmp)) (* 2.0 a)))))
        
;;; problem 3
(define time-to-impact
  (lambda (v e)
    (root1 (/ gravity -2.0) v e)))

(define time-to-height
  (lambda (v e te)
    (time-to-impact v (- e te))))
        

;;; problem 4
(define degree2radian
  (lambda (deg)
    (/ (* deg pi) 180.)))

(define travel-distance-simple
  (lambda (elevation velocity angle) ; angle in degree
    (let ( (vx (* velocity (sin (degree2radian angle))))
           (vy (* velocity (cos (degree2radian angle)))))         
       (position 0 vx 0 (time-to-impact vy elevation)))))

;;; problem 5
(define alpha-increment 0.01)
(define angle-increment 0.01)
(define find-best-angle
  (lambda (velocity elevation)
    (define (recur angle)  ; angle in degree     
      (if (>= angle 90.0) 
          90.0
          (let ((oldangle (recur (+ angle angle-increment))))
            (if (> (travel-distance-simple elevation velocity angle)
                   (travel-distance-simple elevation velocity oldangle))
                angle
                oldangle))))          
    (recur 0)))
          
;;; problem 6
(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* pi .25 (square diameter))))

(define integrate
  (lambda (x0 y0 u0 v0 dt g m beta)
    (if (< y0 0)
        x0
        (let ((speed (sqrt (+ (* u0 u0) (* v0 v0)))))
          (let ((du (/ (* speed u0 dt beta) (- m)))
                (dv (* (+ (/ (* speed beta v0) m) g) (- dt)))
                (dx (* u0 dt))
                (dy (* v0 dt)))
            (integrate (+ x0 dx) (+ y0 dy) (+ u0 du) (+ v0 dv) dt g m beta))))))
            
(define travel-distance
  (lambda(elevation velocity angle)  ;angle in degree
    (integrate 0 elevation 
               (* velocity (cos (degree2radian angle))) 
               (* velocity (sin (degree2radian angle)))
               0.01 gravity mass beta)))

;;; problem 7
(define (best-angle-for-distance elevation velocity distance)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.01))
  (define (try  angle) ; angle in degree
    (if (>= angle 90)
        0
        (let ((dst (travel-distance elevation velocity angle)))
           (if (close-enough? dst distance)
               angle
               (try (+ angle angle-increment))))))
  (try -90))

;;; problem 8
(define (travel-distance-on-bounce e v a bounce-times)
  (define (iter distance v a bounce-times)
    (if (or (< bounce-times 0) (< (abs v) 0.1))
        distance
        (iter (+ distance (travel-distance 0 (* v 0.5) a)) v a (- bounce-times 1))))
  (iter (travel-distance e v a) v a bounce-times))
        
        
           