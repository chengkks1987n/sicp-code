;;; exercise 2.66, the set is represented by a binary tree

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((a-key (key (entry set-of-records))))
	(cond ((= a-key given-key) (entry set-of0records))
	      ((< a-key given-key) 
	       (lookup given-key (right-tree set-of-records)))
	      ((> a-key given-key)
	       (lookup given-key (left-tree set-of-records)))))))
