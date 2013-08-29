;;; exercise 3.24
(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
	  ((same-key? key (caar records)) (car records))
	  (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  false))
	    false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    ((eq? m 'print) local-table)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table eq?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; test 
(put 'letters 'a 97)
(put 'letters 'b 98)
(put 'math '+ 43)
(put 'math '- 42)
(put 'math '* 42)

(get 'math '+)
(get 'letters 'b)

;;; versin 1 for exercise 3.25

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
	  ((same-key? key (caar records)) (car records))
	  (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (let ((record (assoc keys (cdr local-table))))
	(if record
	    (cdr record)
	    false)))
    (define (insert! keys value)
      (let ((record (assoc keys (cdr local-table))))
	(if record
	    (set-cdr! record value)
	    (set-cdr! local-table
		      (cons (cons keys value)
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    ((eq? m 'print) local-table)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; test 
(operation-table 'print)
(put '(letters a b) 10)
(put '(a b c) 11)

(put '(letters a) 97)
(put '(letters b) 98)
(put '(math +) 43)
(put '(math *) 42)

(put '(math) 30)
(put '(english) '31)

(get '(math +))
(get '(letters b))
(get '(letters a))
(get '(math))
(get '(a b c))
(put '(a b c) 12)
(get '(a b c))

;;; version 2 for exercise 3.25
;; ref to: http://community.schemewiki.org/?sicp-ex-3.25

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
	  ((same-key? key (caar records)) (car records))
	  (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lkp keys ptr)
      (if (null? keys) 
	  (cdr ptr)
	  (if (pair? (cdr ptr))
	      (let ((record (assoc (car keys) (cdr ptr))))
		(if record
		    (lkp (cdr keys) record)
		    false))
	      false)))
    (define (lookup keys)
      (lkp keys local-table))    
    (define (ins keys value ptr)
      (if (null? keys)
	  (set-cdr! ptr value)
	  (begin
	    (cond ((not (pair? (cdr ptr))) (set-cdr! ptr (list))))
	    (let ((record (assoc (car keys) (cdr ptr))))
	      (if (not record)
		  (begin
		    (set-cdr! ptr (cons (list (car keys)) (cdr ptr)))
		    (ins (cdr keys) value (car (cdr ptr))))
		  (ins (cdr keys) value record))))))
    (define (insert! keys value)
      (ins keys value local-table)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    ((eq? m 'print) local-table)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define operation-table (make-table eq?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define (lookup keys table) ((table 'lookup-proc) keys))
(define (insert! keys value table) ((table 'insert-proc!) keys value))
;; test 
(operation-table 'print)
(put '(letters a b) 10)
(put '(a b c) 11)
(put '(a b d) 13)
(put '(a b) 14)

(get '(letters a b))

(put '(letters a) 97)
(put '(letters b) 98)
(put '(math +) 43)
(put '(math *) 42)

(put '(math) 30)
(put '(english) '31)

(get '(math +))
(get '(letters b))
(get '(letters a))
(get '(math))
(get '(a b c))
(put '(a b c) 12)
(get '(a b))
(get '(a b c d))
(put '(a b c d) 11)
;;;; ;;;;;;;;;;;;
; versin1 needs more space, and takes more time on each operation.
; it can distigwish the keys '(a b c), '(a b) '(a), while the version2 
; cannot.
;;;;

;;; exercise 3.26
(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	false)))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
	(or previously-computed-result
	    (let ((result (f x)))
	      (insert! x result table)
	      result))))))
(define memo-fib
  (memoize (lambda (n)
	     (cond ((= n 0) 0)
		   ((= n 1) 1)
		   (else (+ (memo-fib (- n 1))
			    (memo-fib (- n 2))))))))
(memo-fib 3)
