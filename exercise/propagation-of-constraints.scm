(define nil (list))

(define (inform-except item lst message)
  (if (not (null? lst))
      (if (eq? item (car lst))
	  (inform-except item (cdr lst) message)
	  (begin ((car lst) message)
		 (inform-except item (cdr lst) message)))))

;-------------------------------------------------------------------

(define (make-connector)
  (let ((value nil)
	(setter nil)
	(constraints nil))
    (define (get-my-value) value)
    (define (has-my-value?) (null? setter))
    (define (forget-my-value! new-setter)
      (if (eq? setter new-setter)
	  (begin (inform-except setter constraints 'I-lost my-value)
		 (set! setter nil))))
    (define (set-my-value! new-value new-setter)
      (if (has-my-value?)
	  (if (not (eq? new-value value))
	      (display "I already have a value!" value  new-value))
	  (begin (set! value new-value)
		 (set! setter new-setter)
		 (inform-except setter constraints 'I-hava-a-value))))
    (define (connect-my cons)
      (if (not (memq cons constraints))
	  (begin (set! constraints (cons cons constraints))
		 (if (has-my-value?)
		     (cons 'I-have-a-value)))))
    (define (me request)
      (cond ((eq? request 'get-value) (get-my-value))
	    ((eq? request 'set-value!) set-my-value!)
	    ((eq? request 'has-value?) (has-my-value?))
	    ((eq? request 'forget-value! forget-my-value!))
	    (else (error "unknow request -- MAKE-CONNECTOR: " request))))
    me))

(define (get-value c) (c 'get-value))
(define (set-value! c value setter) ((c 'set-value!) value setter))
(define (has-value? c) (c 'has-value?))
(define (forget-value! c setter) ((c 'forget-value!) setter))
(define (connect c constraint) ((c 'connect-my) constraint))

;------------------------------------------------------------------

(define (adder a b c)
  <TODO>)

(define (mutiplier a b c)
  <TODO>)

(define (constant v a)
  <TODO>)

;;;;;;;;;;;;;;;;;;;




