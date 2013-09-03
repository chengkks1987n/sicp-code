
;;; exercise 3.28
(define or-gate-delay 5)
(define (logical-or a1 a2)
  (cond ((and (= 0 a1) (= 0 a2)) 0)
	(else 1)))
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-sign a1) (get-sign a2))))
      (after-delay or-gate-delay 
		   (lambda ()
		     (set-sign! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok )

;;; exercise 3.29
;; (or a b) = (not (and (not a) (not b)))
(define (or-gate a b output)
  (let ((c (make-wire))
	(d (make-wire))
	(e (make-wire)))
    (inverter a c)
    (inverter b d)
    (and-gate c d e)
    (inverter e output)))
;; or-gate-delay should be 3*inverter-delay + and-gate-delay

;;; exercise 3.30
(define (half-adder a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (and-gate a b c)
    (or-gate a b d)
    (inverter c e)
    (and-gate d e s))
  'ok)
;; half-adder-delay = 2*add-gate-delay + or-gate-delay + inverter-delay

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out))
  'ok)
;; full-adder-delay = 2*half-adder-delay + or-gate-delay

(define (ripple-carry-adder A B S c)
  (if (null? (cdr A))
      (full-adder (car A) (car B) (make-wire) (car S) c)
      (let ((ck (make-wire)))
	(ripple-carry-adder (cdr A) (cdr B) (cdr S) ck)
	(full-adder (car A) (car B) ck (car S) c)))
  'ok)
;; ripple-carray-adder-delay = n * full-adder-elay

;;; exercise 3.31

;; in the add-gate run the action-procedure will call after-delay which
;; used to initiate the agenda, without it , we cannot do this, the agenda
;; will be empty.

;;; exercise 3.32
(define a1 (make-wire))
(define a2 (make-wire))
(define output (make-wire))
(set-sign! a1 0)
(set-sign! a2 1)
(add-gate a1 a2 output)

(set-sign! a1 1) ; one
(set-sign! a2 0) ; two
;; when set a1 to 1, add action (set-sign! output (and a1(1) a2(1)))
;; then set a2 to 0, add action (set-sign! output (and a1(1) a2(0)))
;; last in,first out will get answer 1.

