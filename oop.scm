p;;; object oriented  program

(define no-method (lambda () 'no-method))

(define (get-method obj msg)
  (if (procedure? obj)
      (obj msg)
      (error obj "is not a procedure object")))

(define (method? m)
  (cond ((not (procedure? m)) #f)
	((eq? m no-method) #f)
	(else #t)))

(define (ask obj msg . args)
  (let ((m (get-method obj msg)))
    (if (method? m)
	(apply m obj args)
	(error "unknow message" msg))))

(define (delegate from to msg . args)
  (let ((m (get-method to msg)))
    (if (method? m)
	(apply m from args)
	(error "cannot get message" msg "from" to))))

(define (is-a? obj type)
  (let ((m (get-method obj type)))
    (method? m)))

(define (make-person fname lname)
  (lambda (msg)
    (case msg
      ((whoareyou)
       (lambda (self) fname))
      ((say)
       (lambda (self something) (display something)))
      ((change-name)
       (lambda (self new-name)
	 (set! fname new-name)
	 (ask self 'say (append '(my new name is ) (ask self 'whoareyou)))))
      ((person?) (lambda (self) #t))	 
      (else no-method))))

(define (make-professor fname lname)
  (let ((int-person (make-person fname lname)))
    (lambda (msg)
      (case msg
	((lecture) 
	 (lambda (self str)
	   (ask self 'say (cons 'therefor str))))
	((whoareyou)
	  (lambda (self) 
	    (cons 'Professor fname)))
	((professor?)
	 (lambda (self) #t))
	(else (get-method int-person msg))))))

(define (make-arrogant-professor fname lname)
  (let ((int-professor (make-professor fname lname)))
    (lambda (msg)
      (case msg
	((say)
	 (lambda (self str)
	   (ask int-professor 'say (append str 'obviously))))
	(else (get-method int-professor msg))))))
	   
	 

;test
(define g (make-person 'ke 'cheng))
(ask g 'say 'hellock)
(ask g 'change-name 'new-ck)
(is-a? g 'person?)
(is-a? g 'professor?)
(is-a? g 'nothing)

;test
(define p (make-professor 'mashel 'Gorge))
(ask p 'whoareyou)
(ask p 'say '(I am good))
(ask p 'lecture '(the sky is blue))
(is-a? p 'person?)
(is-a? p 'professor?)

;test
(define a (make-arrogant-professor 'alice 'Daisy))
(ask a 'whoareyou)
(ask a 'say '(the sky is blue))
(ask a 'lecture '(the sky is blue))
(is-a? a 'person?)
(is-a? a 'professor?)

(delegate a p 'say 'haha)
(delegate p a 'say '(haha))

    

  
