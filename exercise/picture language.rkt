#lang racket

(require graphics/graphics)
(open-graphics)

(define my-viewport (open-viewport "my" 500 500))
;((draw-line my-viewport) (make-posn 12 13) (make-posn 26 36))

(define (my-draw-line v1 v2)
    ((draw-line my-viewport) (make-posn (xcor v1) (ycor v1))
                             (make-posn (xcor v2) (ycor v2))))

(define (draw-frame frame)
  (my-draw-line (org-frame frame)
                (+vect (hor-frame frame) (org-frame frame)))
  (my-draw-line (org-frame frame) 
                (+vect (ver-frame frame) (org-frame frame))))

(define (absolute-vect frame v)
  (define vo (org-frame frame))
  (define vh (hor-frame frame))
  (define vv (ver-frame frame))
  (define vx (+vect (scale-vect vh (xcor v))
                    (scale-vect vv (ycor v))))
  (+vect vx vo))
    
(define (absolute-segment frame s)
  (define v1 (absolute-vect frame (start-point s)))
  (define v2 (absolute-vect frame (end-point s)))
  (make-segment v1 v2))
;;;========================================
(define (draw-segment frame s)
  (define as (absolute-segment frame s))
  (my-draw-line (start-point as) (end-point as)))

(define (draw-picture lines frame)
  (cond ((not (null? lines)) 
         (cond ((not (pair? lines)) (draw-segment frame lines))
               (else (draw-segment frame (car lines))
                     (draw-picture (cdr lines) frame))))))                
;;;==============================================
(define (make-vect x y) (cons x y))
(define (xcor v) (car v))
(define  (ycor v) (cdr v))

(define (make-segment v1 v2) (cons v1 v2))
(define (start-point s) (car s))
(define (end-point s) (cdr s))

(define (make-frame org hor ver) (list org hor ver))
(define (org-frame f) (car f))
(define (hor-frame f) (cadr f))
(define (ver-frame f) (caddr f))
;;;===============================================
(define (+vect v1 v2)
  (make-vect (+ (xcor v1) (xcor v2))
             (+ (ycor v1) (ycor v2))))

(define (scale-vect v ratio)
  (make-vect (* ratio (xcor v))
             (* ratio (ycor v))))

(define (-vect v1 v2)
  (make-vect (- (xcor v1) (xcor v2))
             (- (ycor v1) (ycor v2))))

(define (dot-mutiply v1 v2)
  (+ (* (xcor v1) (xcor v2))
     (* (ycor v1) (ycor v2))))

(define (rotate-vect v angle)
  (let ((c (cos angle))
         (s (sin angle)))
     (make-vect (- (* (xcor v) c) (* (ycor v) s))
                (+ (* (ycor v) c) (* (xcor v) s)))))
                   
;;;================================================
(define p1 (make-vect 0.1 0.5))
(define p2 (make-vect 0.5 0.6))
(define p3 (make-vect 0.8 0.5))
(define p4 (make-vect 0.5 0.3))

(define m1 (make-segment p1 p2))
(define m2 (make-segment p2 p3))
(define m3 (make-segment p3 p4))
(define m4 (make-segment p4 p1))

(define lines (list m1 m2 m3 m4 (make-segment p2 p4)))

(define myframe (make-frame (make-vect 20 30)
                            (make-vect 150 0)
                            (make-vect 0 150)))
;;;============================================
;(draw-picture lines myframe)
;(draw-frame myframe)

(define (make-pic lines)
  (lambda (frame)
    (for-each (lambda (s)
                (draw-segment frame s))
              lines)))

(define (beside pa pb a)
  (lambda (frame)
    (pa (make-frame (org-frame frame)
                    (scale-vect (hor-frame frame) a)
                    (ver-frame frame)))
    (pb (make-frame (+vect (org-frame frame) (scale-vect (hor-frame frame) a))
                    (scale-vect (hor-frame frame) (- 1.0 a))
                    (ver-frame frame)))))

(define (push-right p n a)
  (cond ((<= n 0) p)
        (else (lambda (frame)
                (p (make-frame (org-frame frame)
                               (scale-vect (hor-frame frame) a)
                               (ver-frame frame)))
                ((push-right p (- n 1) a) (make-frame 
                                           (+vect (org-frame frame) (scale-vect (hor-frame frame) a))
                                           (scale-vect (hor-frame frame) (- 1.0 a))
                                           (ver-frame frame)))))))
              
(define (flip-hor p)
  (lambda (frame)
           (p (make-frame (+vect (org-frame frame) (hor-frame frame))
                          (scale-vect (hor-frame frame) -1)
                          (ver-frame frame)))))

(define (rotate90 p)
  (lambda (frame)
           (p (make-frame (+vect (org-frame frame) (hor-frame frame))
                          (ver-frame frame) 
                          (scale-vect (hor-frame frame) -1)))))


(define empty-pic (make-pic null))
(define pic1 (make-pic lines))
(define pic2 (make-pic (list (make-segment p2 p4))))

(draw-frame myframe)
(pic1 myframe)
;((beside pic1 pic1 0.4) myframe)
;((beside pic1 empty-pic 0.5) myframe)
;((push-right pic1 5 0.5) myframe)
;((rotate90 pic1) myframe)
;((rotate90 (rotate90 pic1)) myframe)
((flip-hor pic1) myframe)


