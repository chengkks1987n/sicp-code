(load "../../project/project5/meval.scm")
(load "../../project/project5/environment.scm")
(load "../../project/project5/syntax.scm")
;;; exercise 4.11
(define the-empty-frame '(frame))
(define (make-frame vars vals)
  (define (scan vars vals)
    (if (null? vars) '()
	(cons (list (car vars) (car vals)) (scan (cdr vars) (cdr vals)))))
  (cond ((and (null? vars) (null? vals)) the-empty-frame)
	((and (not (null? vars)) (not (null? vals)))
	 (let ((f (scan vars vals)))
	   (cons 'frame f)))
	(else 
	 (error "vars and vals have diffterent lengths! --- MAKE_FRAME"))))
(define (add-binding-to-frame! var val f)
  (set-cdr! f (cons (list var val) (cdr f))))
(define (lookup-variable-value var env)
  (define (scan bindings)
    (cond ((null? bindings) false)
	  ((eq? (car (car bindings)) var) (cadr (car bindings)))
	  (else (scan (cdr bindings)))))
  (define (env-loop env)
    (if (null? env)
	(error "unbound variable ---"  var)
	(let ((ans (scan (cdr (first-frame env)))))
	  (if ans  ans
	      (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (scan f)
    (cond ((null? f) false)
	  ((eq? (car (car f)) var) 
	   (set-cdr! (car f) (list val))
	   true)
	  (else (scan (cdr f)))))
  (define (env-loop env)
    (if (null? env)
	(error "unbound variable ---"  var)
	(let ((ans (scan (cdr (first-frame env)))))
	  (if ans 'done
	      (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((f (first-frame env)))
    (add-binding-to-frame! var val f)))

;; test
(refresh-global-environment)
(m-eval '(define (f x)
	   (* x x))
	the-global-environment)
(m-eval '(define a 2) the-global-environment)
(m-eval 'a  the-global-environment)
;Value: 2
(m-eval '(f 2) the-global-environment)
;Value: 4


(load "../../project/project5/meval.scm")
(load "../../project/project5/environment.scm")
(load "../../project/project5/syntax.scm")
;;;; exercise 4.12

(define (same-patten env var val next set)
  ;; next -- whether to go to next frame when cannt found the var in current frame
  ;; set  -- wheeher to set new value when found the var
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- " var)
      (let ((f (first-frame env)))
	(let ((p (find-in-frame var f)))
	  (if p
	      (if set
		  (set-cdr! p (list val))
		  (cadr p))
	      (if next
		  (same-patten (enclosing-environment env) var val next set)
		  (add-binding-to-frame! var val f)))))))
(define (set-variable-value! var val env)
  (same-patten env var val true true))
(define (lookup-variable-value var env)
  (same-patten env var '() true false))
(define (define-variable! var val env)
  (same-patten env var val false true))
(refresh-global-environment)

;; test
(m-eval '(define a 11) the-global-environment)
(m-eval '(define (f a)
	   (* a a))
	the-global-environment)
(m-eval 'a the-global-environment)
;Value: 11
(m-eval '(f a) the-global-environment)
;Value: 121
(m-eval '(set! a 12) the-global-environment)
(m-eval 'a the-global-environment)
;Value: 12
(m-eval '(f a) the-global-environment)
;Value: 144


(load "../../project/project5/meval.scm")
(load "../../project/project5/environment.scm")
(load "../../project/project5/syntax.scm")
;;; exercise 4.13
;we  should remove only the binding in the first frame of the environment
;because the enclosing-environment may belong to others
(define (make-unbound! var env)
  (let ((f (first-frame env)))
    (set-cdr! f (del-assq var (cdr f)))))

;; test
(m-eval '(define a 11) the-global-environment)
(m-eval 'a the-global-environment)
;Value: 11
(make-unbound! 'a the-global-environment)
;(m-eval 'a the-global-environment)
; error> 
;Unbound variable -- LOOKUP a
