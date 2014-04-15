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

;;;; exercise 4.12
;(define (lookup-in-frame var f)
;  (define (iter vars vals)
;    (if (null? vars)
;	'()
;	(if (eq? var (car vars))
;	    (car vals)
;	    (iter (cdr vars) (cdr vals)))))
;  (let ((vars (frame-variables f))
;	(vals (frame-values f)))
;    (iter vars vals)))
;
;(define (has-variable-in-frame? var f)
;  (let ((vars (frame-variables f)))
;    (memq var vars)))
;
;(define (set-in-frame! var val f)
;  ;; this code is for binding-implemetation, see exercise 4.11
;  (let ((binding (assq var f)))
;    (if binding
;	(set-cdr! binding val))))
;
;(define (set-in-frame! var val f)
;  ;; this code is for list-implemetation
;  (define (iter vars vals)
;    (if (eq? var (car vars))
;	(set-car! vals val)
;	(iter (cdr vars) (cdr vals))))
;  (let ((vars (car f))
;	(vals (cadr f)))
;    (iter vars vals)))
;
;(define (lookup-variable-value var env)
;  ((member-procedure (lambda (f)
;		       (not (null? f))))
;   (map lookup-in-frame env)))
;
;(define (define-variable! var val env)
;  (if (has-variable-in-frame (first-frame env))
;      (set-in-frame! var val (first-frame env))
;      (add-binding-to-frame var val (first-frame env))))
;
;(define (set-variable! var val env)
;  ((member-procedure (lambda (f)
;		       (if (has-variable-in-frame? var f)
;			   (begin
;			     (set-in-frame var val env)
;			     'true)
;			   'false)))
;   env))
;
;;;; exercise 4.13
;(define (del-from-frame! var f)
;  (define (iter vars vals)
;    (if (null? vars)
;	'()
;	(if (eq? var (car vars))
;	    (begin (set! vars (cdr vars))
;		   (Set! vals (cdr vals))
;		   'OK)
;	    (iter (cdr vars) (cdr vals)))))
;  (iter (frame-variables f) (frame-values f)))
;
;(define (make-unbound! var env)
;  (let ((f (first-frame env)))
;    (if (null? (del-from-frame! var f))
;	(make-unbound! var (enclosing-enviroment env)))))
