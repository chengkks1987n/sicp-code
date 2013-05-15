#lang scheme

;;;exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch structure)
  (car structure))

(define (right-branch structure)
  (cadr structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight structure) 
    (cond ((not (pair? structure)) structure)         
          (else (+ (total-weight (branch-structure (left-branch structure)))
                   (total-weight (branch-structure (right-branch structure)))))))


(define (balance? structure)
   (cond ((not (pair? structure)) true)
         (else (let ((lb (left-branch structure))
                     (rb (right-branch structure)))
                 (let ((ls (branch-structure lb))
                       (rs (branch-structure rb)))
                   (cond ((not (balance? ls)) false)
                         ((not (balance? rs)) false)
                         (else ( = (* (total-weight ls)
                                      (branch-length lb))
                                   (* (total-weight rs)
                                      (branch-length rb))))))))))

(define b1 (make-branch 4 3))
(define b2 (make-branch 2 5))
(define b3 (make-branch 2 3))
(define m12 (make-mobile b1 b2))
(define b12 (make-branch 3 m12))
(define m12-3 (make-mobile b12 b3))
(total-weight m12)
(total-weight m12-3)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

  
 (define level-1-mobile (make-mobile (make-branch 2 1) 
                                     (make-branch 1 2))) 
 (define level-2-mobile (make-mobile (make-branch 3 level-1-mobile) 
                                     (make-branch 9 1))) 
 (define level-3-mobile (make-mobile (make-branch 4 level-2-mobile) 
                                     (make-branch 8 2))) 


(balance? level-1-mobile) 
 (balance? level-2-mobile) 
 (balance? level-3-mobile) 
  
 (balance? (make-mobile (make-branch 10 1000) 
                         (make-branch 1 level-3-mobile))) 
