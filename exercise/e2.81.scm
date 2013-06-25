(put-coercion 'scheme-number 'complex scheme-number->complex)
(get-coercion 'scheme-number 'complex )

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(let ((t1->t2 (get-coercion type1 type2))
		      (t2->t1 (get-coercion type2 type1)))
		  (cond (t1->t2
			 (apply-generic op (t1->t2 a1) a2))
			(t2->t1
			 (apply-generic op a1 (t2->t1 a2)))
			(else
			 (error "No method for these types"
				(list op type-tags))))))
	      (error "No method for these types"
		     (list op type-tags)))))))
;;; exercise 2.81
;;; a - loop forever
;;; b - he is right, apply-generic does not work
;;     for example, we define operations on (complex number), but we call 
;;     a fuction on (number number)
;;; c - 

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(if (eq? type1 type2)
		    (error "No method for these types" (list op type-tags))
		    (let ((t1->t2 (get-coercion type1 type2))
			  (t2->t1 (get-coercion type2 type1)))
		      (cond (t1->t2
			     (apply-generic op (t1->t2 a1) a2))
			    (t2->t1
			     (apply-generic op a1 (t2->t1 a2)))
			    (else
			     (error "No method for these types"
				    (list op type-tags)))))))
		(error "No method for these types"
		       (list op type-tags)))))))


;;; exercise 2.82
  ;; apply op on new args, if cannot return false;
  (define (apply-on-new-args op new-args)
    (let ((types (map type-tag new-args)))
      (let ((proc get op types))
	(if proc
	    (apply proc (map (contents new-args)))
	    false))))

(define (apply-generic op . args)
  ;; convert all args to a type, if cannot return false;
  (define (args->type args type)
    (if (null? args)
	(list)
	(let ((t1 (arg-type (car args))))
	  (if (eq? t1 type)
	      (cons (car args) (args-type (cdr args) type))
	      (let ((t1->t2 (get-coercion t1 type)))
		(if t1->t2
		    (cons (t1->t2 (car args)) (args-type (cdr args) type))
		    false))))))

  ;; 
  (define (apply-on-args args types)
    (if (null? types)
	false
	(let ((t2 (car type)))
	  (let ((new-args (args->type args (car types))))
	    (let ((ans (apply-on-new-args op new-args)))
	      (if ans
		  ans
		  (apply-on-args args (cdr types))))))))
  ;; 
  (let ((ans (apply-on-new-args op args))
	(types (map type-tag args)))
    (if ans
	ans
	(let ((ans2 (apply-on-args args types)))
	  (if ans2
	      ans2
	      (error "No method for these types"
		     (list op types)))))))

;;; exercise 2.83
;;; in order to predicate whether a type is highest, we can make sure that :
;;; if the input is highest type, return the input
;;; else return the value which is one level higher than input
;;; 
(define (int->rat value)
  (make-rational value 1))

(define (rat->real value)
  (make-real (/ (* 1.0 (numer value)) (denom value))))

(define (real->complex value)
  (make-complex-from-real-img value 0))

(put-coercion 'integer 'rational int->rat)
(put-coercion 'rational 'real rat->real)
(put-coercion 'real 'complex real->complex)
		       
(define types-tower '(interge rational real complex))

(define (raise value)
  (define (iter x tower)
    (if (or (null? tower) (null? (cdr tower)))
	x
	(let ((t1 (car tower)))
	  (if (eq? t1 (type-tag x))
	      ((get-coercion t1 (cadr tower)) x)
	      (iter x (cdr tower))))))
  (iter value types-tower))
;; another way for this problem
(put 'raise 'interger int->rat)
(put 'raise 'rational rat->real)
(put 'raise 'real real->complex)
(put 'raise 'comple (lambda (x) x))

(define (raise x)
  (apply-generic 'raise x))

;;; exercise 2.84
;; whether value x has the highest type?
(define (has-highest-type? x)
  (eq? (type-tag x) 'complex))

;; if the type of value1 is equal to that of value2, return 0
;; if bigger ,return 1;
;; else return -1;
(define (compare-type value1 value2)
  (define (help value1 value2 raised)
    (let ((t1 (type-tag value1))
	  (t2 (type-tag value2)))
      (if (and (eq? t1 t2) raised)
	  (if raised -1 0)	      
	  (if (highest-type? (type-tag value1))
	      -1
	      (help (raise value1) value2 true)))))
  (help value1 value2 false))

(define (get-op-on-args op args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc proc false))))

;; wether the args have the same type
(define (all-same-type? args)
  (if (or (null? args) (null? (cdr args)))
      true
      (if (= 0 (compare-type (car args) (cadr args)))
	  (all-same-type? (cdr args))
	  false)))

(define (raise-all-to-same-level args)
  (define (help args)
    (if (or (null? args) (null? (cdr args)))
	args
	(cond ((= 0 (compare-type (car args) (cadr args)))
	       (cons (car args) (help (cdr args))))
	      ((< 0 (compare-type (car args) (cadr args)))
	       (cons (car args) (cons (raise (cadr args)) (help cddr args))))
	      (else
	       (cons (raise (car args)) (help (cdr args)))))))
    
  (if (all-same-type?  args)
      args
      (raise-all-to-same-level (help args))))
  
	    
(define (apply-generic op . args)
  (define (continue-raise new-args)
    (if (highest-type? (car new-args))
	(error "cannot apply op on args")
	(let ((next-args (map raise new-args)))
	  (let ((proc (get-op-on-args op next-args)))
	    (if proc
		(apply proc (map contests next-args))
		(continue-raise next-args))))))

  (let ((proc (get-op-on-args tags)))
    (if proc
	(apply proc (map contents args))
	(let ((new-arg (raise-all-to-same-level args)))
	  (let ((proc (get-op-on-args new-args)))
	    (if proc
		(apply proc (map contents new-args))
		(continue-raise new-args)))))))

;;; exercise 2.85  
(put 'project 'complex (lambda (x) (real-part x)))
(put 'project 'rational (lambda (x) (make-rational (floor x) 1)))
(put 'project 'rational (lambda (x) (numer x)))

(define (project x) (apply-generic 'project x))

(define (drop x)
  (if (equ? x (raise (project x)))
      (drop (project x))
      x))
      



