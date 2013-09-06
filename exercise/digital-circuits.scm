
;;; utilily
(define (call-each procs)
  (if (null? procs)
      'done
      (begin ((car procs))
	     (call-each (cdr procs)))))

(define (call-action-queue action-queue)
  (if (queue-empty? action-queue)
      'done
      (let ((action (dequeue! action-queue)))
	(action)
	(call-action-queue action-queue))))

;;; agenda
(define (make-agenda)
  (let ((current-time 0)
	(schedule (make-1d-table)))
    (define (empty-agenda?)
      (let ((lst (1d-table/alist schedule)))
	(null? lst)))

    (define (add-agenda-action! delay-time action)
      (let ((time (+ current-time delay-time)))
	(let ((queue (1d-table/get schedule time #f)))
	  (if queue
	      (enqueue! queue action)
	      (let ((queue (make-queue)))
		(enqueue! queue action)
		(1d-table/put! schedule time queue))))))
    
    (define (pop-next-action-queue!)
      (let ((queue (1d-table/get schedule current-time #f)))
	(if queue
	    (begin
	      (1d-table/remove! schedule current-time)
	      (set! current-time (+ 1 current-time))
	      queue)
	    (begin
	      (set! current-time (+ 1 current-time))
	      (pop-next-action-queue!)))))

    (define (propagate)
      (if (empty-agenda?)
	  (begin (set! current-time 0)
		 'done)
	  (let ((action-queue (pop-next-action-queue!)))
	    (call-action-queue action-queue))))	    
      
    (define (dispach m)
      (cond ((eq? m 'add-agenda-action!) add-agenda-action!)
	    ((eq? m 'propagate) (propagate))
	    ((eq? m 'current-time) current-time)
	    ((eq? m 'empty?) (empty-agenda?))
	    (else (error "unkown message in agenda" m))))
    dispach))

(define the-agenda (make-agenda))
(define (after-delay delay-time action) 
  ((the-agenda 'add-agenda-action!) delay-time action))
(define (propagate) (the-agenda 'propagate))
(define (current-agenda-time) (the-agenda 'current-time))

;;; primary box 
(define and-gate-delay 3)
(define or-gate-delay 5)
(define not-gate-delay 2)

(define (logical-and a b) (if (and (= 1 a) (= 1 b)) 1 0))
(define (logical-or a b) (if (and (= 0 a) (= 0 b)) 0 1))
(define (logical-not a) (if (= a 0) 1 0))

(define (and-gate a b output)
  (define (and-gate-proc)
    (after-delay and-gate-delay 
		 (lambda ()
		   (set-sign! output
			      (logical-and (get-sign a) (get-sign b))))))
  (add-action! a and-gate-proc)
  (add-action! b and-gate-proc)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-sign a1) (get-sign a2))))
      (after-delay or-gate-delay 
		   (lambda ()
		     (set-sign! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok )

(define (not-gate a output)
  (define (not-gate-proc)
    (after-delay not-gate-delay
		 (lambda ()
		   (set-sign! output (logical-not (get-sign a))))))
  (add-action! a not-gate-proc)
n  'ok)

;;; half-adder and full-adder
;;<TODO:>

;;; wire
(define (make-wire)
  (let ((sign 0)
	(procs (list)))
    (define (get-sign) sign)
    (define (set-sign! new-value)
      (if (= sign new-value)
	  'done
	  (begin 
	    (set! sign new-value)
	    (call-each procs))))
    (define (add-action! action-proc)
      (set! procs (cons action-proc procs))
      (action-proc))
    (define (dispach m)
      (cond ((eq? m 'get-sign) get-sign)
	    ((eq? m 'set-sign!) set-sign!)
	    ((eq? m 'add-action!) add-action!)
	    (else
	     (error "error in make-wire dispach: unknow message" m))))
    dispach))

(define (get-sign wire) ((wire 'get-sign)))
(define (set-sign! wire new-value) ((wire 'set-sign!) new-value))
(define (add-action! wire action-proc) ((wire 'add-action!) action-proc))

;;; example
(define (probe name wire)
  (add-action! wire 
	       (lambda ()
		 (newline)
		 (display name)
		 (display " ")
		 (display (current-agenda-time))
		 (display " new-value ")
		 (display (get-sign wire))
		 (newline))))

(define a (make-wire))
(define b (make-wire))
(define output (make-wire))
(define c (make-wire))

(probe 'a a)
(probe 'b b)
(probe 'c c)
(probe 'output output)

(set-sign! a 1)

(and-gate a b output)
(or-gate a b c)
(propagate)
