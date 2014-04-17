(load "meval.scm")
(load "environment.scm")
(load "syntax.scm")
;;; exercise 4.14

;;; primivate-procedure map 
;;; see in file environment.scm
;(m-eval '(pmap car '((a 1) (b 2) (c 3))) the-global-environment)
; error>
;The object (primitive #[compiled-procedure 11 ("list" #x1) #xf #x321823]) is not applicable.

;(m-eval '(pmap car '((a 1) (b 2) (c 3))) the-global-environment) is converted
; to (apply map '(car '((a 1) (b 2) (c 3)))), the 'car is just a symbol not 
; a primivate-procedure for scheme. it should be (apply map (list car '((a 1) ..

;; because the map is a high order procedure, one of the arguments of map is
; also a procedure!  the procedure argument is applyed to scheme-apply just
;  as symbol, not evaluated by self-define apply, so things go wrong.

;;;  self-define map
(m-eval '(define (my-map pro lst)
	   (if (null? lst)
	       '()
	       (cons (pro (car lst)) (my-map pro (cdr lst)))))
	the-global-environment)
;; test
(m-eval '(my-map car '((a 1) (b 2) (c 3))) the-global-environment)
;Value 16: (a b c)

;;;;; exercise 4.15
; there are only two possible outcomes for the expression (try try) :
; 1. run forever 
;    if the outcome is 'run forever', the result of (halt? p p) must be
;    true, which means (try try) is halted, we get the paradox!
; 2. halted
;    if the outcome is halted, the result of (halt? p p) must be false,
;    which means (try try) is run-forever, we get the paradox!
; after all, we come to the conclution : the procedure halt? is impossible.


