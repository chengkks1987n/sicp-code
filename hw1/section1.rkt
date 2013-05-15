#lang scheme

;; Section 1
;; Question 1
(define (*-helper a b)
  (define (iter ans b)
    (if (= 0 b)
        ans
        (iter (+ ans a) (- b 1))))
  (iter 0 b))
;test 1
;(*-helper 3 4)
;(*-helper 1 4)
;(*-helper 3 1)
;(*-helper 1 1)
;(*-helper 3 0)
;(*-helper 0 4)
;(*-helper 0 0)

;; Question 2
(define (^-helper a b)
  (define (iter ans b)
    (if (= b 0)
        ans
        (iter (*-helper ans a) (- b 1))))
  (iter 1 b))
;test 2
;(^-helper 4 0)
;(^-helper 4 1)
;(^-helper 4 3)
;(^-helper 1 3)
;(^-helper 1 0)
;(^-helper 0 3)
;(^-helper 0 0)  dont make any sence
  

;; Question 3
(define (/-helper a b)
    (define (iter q r y)
      (if (< r y)
          (cons q r)
          (iter (+ 1 q) (- r y) y)))
  (cond ((= a 0) (cons 0 0))
        ((= b 0) null)        
        (else (let ((ans (iter 0 (abs a) (abs b))))
                (cond ((and (> a 0) (> b 0)) ans)
                      ((and (> a 0) (< b 0)) (cons (- (car ans)) (cdr ans)))
                      ((and (< a 0) (> b 0)) (cons (- -1 (car ans)) (- b (cdr ans))))
                      ((and (< a 0) (< b 0)) (cons (+ (car ans) 1) (- (- b) (cdr ans)))))))))
;test 3
;(/-helper 3 7)
;(/-helper 7 3)
;(/-helper 7 0)
;(/-helper 0 3)
;(/-helper 6 3)
;(/-helper -7 3)
;(/-helper 7 -3)
;(/-helper -7 -3)
        
  
;; Question 4
;use list to represent long numbers
;the low digits in the front of list
(define base 10)
(define (n2list n ans)
    (if (< n base)
        (append ans (list n))
        (n2list (quotient n base) (append ans (list (remainder n base))))))
  
  (define (list+number lst n ans) 
    (if (= 0 n) 
        (append ans lst)
        (let ((nlst (n2list n null)))
          (list+list lst nlst 0 ans))))
  
  (define (list+list lst1 lst2 c ans)
    (cond ((and (empty? lst1) (empty? lst2) (= c 0)) ans)
          ((and (empty? lst1) (empty? lst2) (not (= c 0))) (append ans (n2list c null)))
          ((and (empty? lst1) (not (empty? lst2))) (append ans (list+number lst2 c null)))           
          ((and (empty? lst2) (not (empty? lst1))) (list+list lst2 lst1 c ans))
          (else (let ((z (+ c (first lst1) (first lst2))))
                  (list+list (rest lst1) (rest lst2) (quotient z base) 
                             (append ans (list (remainder z base))))))))
  
  (define (list*number lst n ans)
    (cond ((= n 0) (list 0))
          ((empty? lst) ans)
          (else (let ((z (* (first lst) n)))
                  (append ans 
                          (list (remainder z base)) 
                          (list+number (list*number (rest lst) n null) (quotient z base) null))))))
  
(define (list*list lst1 lst2)
  (let ((x0 (first lst1))
        (x1 (rest lst1))
        (y0 (first lst2))
        (y1 (rest lst2)))
    (cond ((and (empty? x1) (empty? y1)) (n2list (* x0 y0) null))
          ((and (empty? x1) (not (empty? y1))) 
           (list+list (n2list (* x0 y0) null) (list*number y1 (* base x0) null) 0 null))
          ((and (empty? y1) (not (empty? x1))) (list*list lst2 lst1))
          (else (list+list 
                 (list+list (n2list (* x0 y0) null) (list*number y1 (* base x0) null) 0 null)
                 (list+list (list*number x1 (* base y0) null)
                           (list*number (list*list x1 y1) (* base base) null)
                           0 null)
                 0 null)))))  
                       
(define (karatsuba  lst1 lst2)   
  (list*list lst1 lst2))

          
           
    
 