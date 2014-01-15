;;;;;;;;;;;;;;;
;; the solution for quiz 2

(define (list->cycle lst)
  (set-cdr! (last lst) lst)
  lst)

(define (last lst)
  (if (null? lst)
      (error "not long enough")
      (if (null? (cdr lst))
	  lst
	  (last (cdr lst)))))

(define head car)

(define test-cycle (list->cycle '(a b c g)))  

(head test-cycle)
;Value: a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; part 1
;; question 1
(define rotate-left cdr)

(head (rotate-left test-cycle))
;Value: b
(head (rotate-left (rotate-left test-cycle)))
;Value: c

;; question 2
(define (rotate-right cycle)
  (define (helper cur)
    (if (eq? (cdr cur) cycle)
	cur
	(helper (cdr cur))))
  (helper cycle))

(head (rotate-right test-cycle))
;Value: g

;; question 3
(define (insert-cycle! cycle elem)
  (let ((tail (rotate-right cycle))
	(tmp (cons elem cycle)))
    (set-cdr! tail tmp)
    'done))

(head test-cycle)
;Value: a

(insert-cycle! (rotate-left (rotate-left test-cycle)) 'x)
;Value: done

(head test-cycle)
;Value: a

(head (rotate-left test-cycle))
;Value: b

(head (rotate-left (rotate-left test-cycle)))
;Value: x

(head (rotate-left (rotate-left (rotate-left test-cycle))))
;Value: c

;; question 4
(define (delete-cycle! cycle)
  (let ((next (rotate-left cycle)))
    (set-cdr! cycle (rotate-left next))
    (set-cdr! next '())
    'deleted))

(delete-cycle! test-cycle)
;Value: deleted

(head test-cycle)
;Value: a

(head (rotate-left test-cycle))
;Value: x

(define (delete-cycle! cycle)
  (let ((r (rotate-right cycle))
	(l (rotate-left cycle)))
    (set-cdr! r l)
    (Set-cdr! cycle '())
    'deleted))

(define new-cycle (rotate-left test-cycle))

(delete-cycle! test-cycle)
;Value: deleted

(head test-cycle)
;Value: a
(cdr test-cycle)
;Value: ()

(head new-cycle)
;Value: x
(head (rotate-left new-cycle))
;Value: c

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; part 2
; I don't have the partial environment diaggram.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; part 3
;; see the solution.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; question 15
(define sheets-for-floder
  (lambda ()
    (fold-right + 0
		(map (lambda (t) (ask t 'sheets)) contents))))


;; question 19
(define (aged-cabinet self name)
  (let ((cabinet-part (cabinet self name))
	(age 0))
    (make-handler
     'aged-cabinet
     (make-methods
      'add-thing
      (lambda (t)
	(if (> age 4)
	    'broken
	    (begin 
	      (ask cabinet-part 'add-thing t)
	      (set! age (+ 1 age)))))
      )
     cabinet-part)))

;; question 20
(define (located-cabinet self name x y)
  (let ((cabinet-part (cabinet self name))
	(located-part (located-object self x y)))
    (make-handler
     'located-cabinet
     (make-methods)
     cabinet-part located-part)))

;; question 21
(make-methods
 'set-x!
 (lambda (newx)
   (display "My location has changed")
   (newline)
   (ask 'located-part 'set-x! newx)))
    
;; question 22
; no
(make-methods
 'set-x-y!
 (lambda (nx ny)
   (ask self 'set-x! nx)
   (ask self 'set-y! ny)))

