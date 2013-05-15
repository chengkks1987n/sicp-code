#lang racket

(define nil null)
;;;========= constructors and selectors ================
(define (make-entry p s pos)
  (list p s pos))

(define person car)
(define salary cadr)
(define position caddr)
(define names person)

(define make-person list)
(define family-name car)
(define given-names cdr)

;;;================== sample data =====================

(define  sampledata
  (list (make-entry (make-person "smith"  "john"  "henry")  30000 "president")
        (make-entry (make-person "jones" "anne" "marie" "heather")  60000 "hacker") 
        (make-entry (make-person "smith"  "fred") 55000 "hacker")
        (make-entry  (make-person "doe" "jane" "elizabeth") 38000 "assistant")
        (make-entry (make-person "roe"  "marie" "jane") 29000 "vice-president")))

(define test (make-entry (make-person "smith"  "john"  "henry")  30000 "president"))

;;;=============== functions =======================

(define (filter pred lst)
  (cond ((null? lst) null)
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (called-by name)
  (lambda (entry)
    (string=? name (car (given-names (person entry))))))

(define (salary-and-position mininum posn)
  (lambda (entry)
    (if (string=? posn (position entry))
        (>= (salary entry) mininum)
        false)))

(define (one-of name lst)
  (cond ((null? lst) false)
        ((string=? name (car lst)) true)
        (else (one-of name (cdr lst)))))

(define (has-name name)
  (lambda (entry)
    (one-of name (given-names (person entry)))))

;;(define (map proc lst)
;;  (if (null? lst)
;;      null
;;      (cons (proc (car lst)) (map   proc (cdr lst)))))

(define (INSERT2 entry)
  (length (person entry)))
;;;============= test cases ==================
;(person test)
;(salary test)
;(position test)

;(given-names (make-person "jones" "anne" "marie" "heather"))
;(family-name (make-person "jones" "anne" "marie" "heather"))

;(given-names (person test))
;(family-name (person test))

;(filter (called-by "jane") sampledata)

;(filter (salary-and-position 60000 "hacker") sampledata)

;(filter (has-name "marie") sampledata)

;(map INSERT2 sampledata)

;;; =============== part 4 ================
(define (find-best best rest compare extractor) 
  (if (null? rest) 
      best
      (if (compare (extractor (car rest)) (extractor best))
          (find-best (car rest) (cdr rest) compare extractor)
          (find-best best (cdr rest)  compare extractor))))

(define (remove elt rest same) 
  (if (null? rest)
      nil
      (if (same elt (car rest))
          (cdr rest)
          (cons (car rest)  (remove elt (cdr rest) same)))))

(define (sort  data compare extractor same)
  (let ((trial (find-best (car data) (cdr data) compare extractor)))
    (let ((rest (remove trial data same)))
      (if (null? rest) (list trial)
          (cons trial (sort rest compare extractor same))))))

;(sort sampledata <  salary  =)  ???????

;;; =========== part 5 ==============

(define (compose f g) (lambda (x)  (f (g x))))

(define (repeated f n) 
  (if  (= n  1)
       f
       (lambda (x)
         ((repeated f (- n 1)) (f x)))))

(define (square x) (* x x))

;square
;(repeated square 4)
;((repeated square 3) 2)
;((repeated square 4) 2)
;((repeated square 2) 3)

;;; ===== part 6 ========

(define  (gcd x  y) 
  (assert x  number?)
  (assert y  (list number? (lambda  (a) (>=   a 0))))
  (if  (= y  0)
       x
       (gcd y  (remainder x  y))))

(define (assert test-val procs) 
  (cond ((procedure? procs) (if (procs test-val) 
                                true
                                (error "error")))
        ((null? procs) #T)
        (else (if ((car procs) test-val)
                  (assert test-val (cdr procs))
                  (error "error happend")))))
        






