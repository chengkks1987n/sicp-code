#lang scheme

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum ((deriv (addend exp) var)
                    (deriv (augend exp) var))))
        ((product? exp)
         (make-sum 
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (mutiplicand exp)
                        (deriv (mutiplier exp) var))))
        (else (error "[DERIV] unknow expression type" exp))
        
             
       