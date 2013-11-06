(define the-empty-stream '())

(define (stream-null? s) (null? s))

;; memo-proc, (void -> T) - > (void -> T)
;; take a procedure with no argument as input,
;; return a procedur with no argument.
;; the first time you call the returned procedure,
;; it call the input procedur and return its result, also memory
;; its result.
;; next time you call it, just return the result.
;; 
;; it is effective.
;;
;; proc, void -> T, a procedure with no argument 
(define (memo-proc proc)
  (let ((already-run false) (result false))
    (lambda ()
      (if (not already-run)
	  (begin (set! result (proc))
		 (display "now running")
		 (newline)
		 (set! already-run true)
		 result)
	  result))))

;; test for memo-proc
(define (t-proc) 
  (display "test procedue - no retrun value")
  (newline))

(define (n-proc) 
  (display "another test procedue - have retrun value")
  (newline)
  'done)

(define m-t (memo-proc t-proc))
(define m-n (memo-proc n-proc))
(m-n)
;=> another test procedue - have retrun value
;=> now running
;=> ;Value: done

(m-n)
;=> ;Value: done
(m-t)
;=> test procedue - no retrun value
;=> now running
;=> ;Unspecified return value

(m-t)
;=> ;Unspecified return value
;;------------------------------

;(define con-strea 
; <TODO>
(define cons-stream cons)  

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))


;;; exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-cdr argstreams))))))


