;;; exercise 2.77

(put <op> <type> <item>)
(get <op> <type>)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types -- APPLY-GENERIC"
	   (list op type-tags))))))

(define (magnitude z) (apply-generic 'magnitude z))
