 ;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;	    

(define *game-association-list*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))



(define (play-loop strat0 strat1 strat2)
  (define (play-loop-iter strat0 strat1 strat2  count 
			  history0 history1 history2 limit)
    (cond ((= count limit) (print-out-results
			    history0 history1 history2 limit))
	  (else (let ((result0 (strat0 history0 history1 history2))
		      (result1 (strat1 history1 history0 history2))
		      (result2 (strat2 history2 history1 history0)))
		  (play-loop-iter strat0 strat1 strat2 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  (extend-history result2 history2)
				  limit)))))
  (play-loop-iter strat0 strat1 strat2 0
		  the-empty-history the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 history2 number-of-games)
  (let ((scores (get-scores history0 history1 history2)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (display "Player 3 Score:  ")
    (display (* 1.0 (/ (caddr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1 history2)
  (define (get-scores-helper history0 history1 history2
			     score0 score1 score2)
    (cond ((empty-history? history0)
	   (list score0 score1 score2))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1)
				       (most-recent-play history2))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (rest-of-plays history2)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1)
				     (+ (get-player-points 2 game) score2)
				     )))))
  (get-scores-helper history0 history1 history2 0 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

;; note that you will need to write extract-entry

(define make-play list)

;; pperators on history
(define the-empty-history '())
(define extend-history cons)
(define empty-history? null?)
(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

;;(define (EGALITARIAN  my-history other-history)
;;  (define (count-instances-of test hist)
;;    (cond ((empty-history? hist) 0)
;;	  ((string=? (most-recent-play hist) test)
;;	   (+ (count-instances-of test (rest-of-plays hist)) 1))
;;	  (else (count-instances-of test (rest-of-plays hist)))))
;;  (let ((ds (count-instances-of "d" other-history))
;;	(cs (count-instances-of "c" other-history)))
;;    (if (> ds cs) "d" "c")))
;;
;;(define (EYE-FOR-EYE my-history other-history)
;;  (if (empty-history? my-history)
;;      "c"
;;      (most-recent-play other-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 9
; - change procedure play-loop, print-out-results, get-scores, 
; - remove EYE-FOR-EYE , EGALITARIAN, nasty, patsy, spastic
; 
(define (equal-play? play1 play2)
  (cond ((and (null? play1) (null? play2)) true)
	((and (not (null? play1)) (not (null? play2))) 
	 (if (string=? (car play1) (car play2))
	     (equal-play? (cdr play1) (cdr play2))
	     false))
	(else false)))

(define (extract-entry game game-association-list)
  (cond ((null? game-association-list)
	 (error "cannot find game in the list"))
	((equal-play? game (car (car game-association-list))) 
	 (car game-association-list))
	(else (extract-entry game (cdr game-association-list)))))


;; problem 10
(define (NASTY-3 my-history history1 history2)
  "d")

(define (PATSY-3 my-history history1 history2)
  "c")

(define (SPASTIC-3 my-history history1 history2)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (soft-eye-for-eye my-history history1 history2)
  (if (and (not (empty-history? history1))
	   (not (empty-history? history2))
	   (string=? "d" (car history1))
	   (string=? "d" (car history2)))
      "d"
      "c"))

(define (tough-eye-for-eye my-history history1 history2)
  (cond ((and (not (empty-history? history1))
	      (not (empty-history? history2))
	      (string=? "c" (car history1))
	      (string=? "c" (car history2)))
	 "c")
	((and (empty-history? history1)
	      (empty-history? history2))
	 "c")
	(else "d")))

;test
(NASTY-3 "a" 'd 'd)
(PATSY-3 's 's 'e)
(SPASTIC-3 'a 'b 'c)

(NASTY-3 (list "c") (list "c") (list "c"))
;; ==> "d"
(NASTY-3 (list "c") (list "c") (list "d"))
;; ==> "d"
(NASTY-3 (list "c") (list "d") (list "c"))
;; ==> "d"
(NASTY-3 (list "c") (list "d") (list "d"))
;; ==> "d"
(NASTY-3 (list "d") (list "c") (list "c"))
;; ==> "d"
(NASTY-3 (list "d") (list "c") (list "d"))
;; ==> "d"
(NASTY-3 (list "d") (list "d") (list "c"))
;; ==> "d"
(NASTY-3 (list "d") (list "d") (list "d"))

(PATSY-3 (list "c") (list "c") (list "c"))
;; ==> "c"
(PATSY-3 (list "c") (list "c") (list "d"))
;; ==> "c"
(PATSY-3 (list "c") (list "d") (list "c"))
;; ==> "c"
(PATSY-3 (list "c") (list "d") (list "d"))
;; ==> "c"
(PATSY-3 (list "d") (list "c") (list "c"))
;; ==> "c"
(PATSY-3 (list "d") (list "c") (list "d"))
;; ==> "c"
(PATSY-3 (list "d") (list "d") (list "c"))
;; ==> "c"
(PATSY-3 (list "d") (list "d") (list "d"))

(SPASTIC-3 (list "c") (list "c") (list "c"))

(TOUGH-EYE-FOR-EYE (list "c") (list "c") (list "c"))
;; ==> "c"
(TOUGH-EYE-FOR-EYE (list "c") (list "c") (list "d"))
;; ==> "d"
(TOUGH-EYE-FOR-EYE (list "c") (list "d") (list "c"))
;; ==> "d"
(TOUGH-EYE-FOR-EYE (list "c") (list "d") (list "d"))
;; ==> "d"
(TOUGH-EYE-FOR-EYE (list "d") (list "c") (list "c"))
;; ==> "c"
(TOUGH-EYE-FOR-EYE (list "d") (list "c") (list "d"))
;; ==> "d"
(TOUGH-EYE-FOR-EYE (list "d") (list "d") (list "c"))
;; ==> "d"
(TOUGH-EYE-FOR-EYE (list "d") (list "d") (list "d"))
;; ==> "d"

(SOFT-EYE-FOR-EYE (list "c") (list "c") (list "c"))
;; ==> "c"
(SOFT-EYE-FOR-EYE (list "c") (list "c") (list "d"))
;; ==> "c"
(SOFT-EYE-FOR-EYE (list "c") (list "d") (list "c"))
;; ==> "c"
(SOFT-EYE-FOR-EYE (list "c") (list "d") (list "d"))
;; ==> "d"
(SOFT-EYE-FOR-EYE (list "d") (list "c") (list "c"))
;; ==> "c"
(SOFT-EYE-FOR-EYE (list "d") (list "c") (list "d"))
;; ==> "c"
(SOFT-EYE-FOR-EYE (list "d") (list "d") (list "c"))
;; ==> "c"
(SOFT-EYE-FOR-EYE (list "d") (list "d") (list "d"))
;; ==> "d"

;;; problem 11
(define (make-combined-strategies strat0 strat1 combine-proc)
  (lambda (my-history history1 history2)
    (let ((r0 (strat0 history1 history2))
	  (r1 (strat1 history2 history1)))
      (combine-proc r0 r1))))

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

(define new-tough (make-combined-strategies 
		   EYE-FOR-EYE EYE-FOR-EYE
		   (lambda (r1 r2) 
		     (if (or (string=? "d" r1)
			     (string=? "d" r2))
			 "d" "c"))))

(new-tough (list "c") (list "c") (list "c"))
;; ==> "c"
(new-tough (list "c") (list "c") (list "d"))
;; ==> "d"
(new-tough (list "c") (list "d") (list "c"))
;; ==> "d"
(new-tough (list "c") (list "d") (list "d"))
;; ==> "d"
(new-tough (list "d") (list "c") (list "c"))
;; ==> "c"
(new-tough (list "d") (list "c") (list "d"))
;; ==> "d"
(new-tough (list "d") (list "d") (list "c"))
;; ==> "d"
(new-tough (list "d") (list "d") (list "d"))
;; ==> "d"

(define (EGALITARIAN  my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

(define egalitarian=eye (make-combined-strategies
			 EYE-FOR-EYE
			 EGALITARIAN
			 (lambda (r1 r2)
			   (if (= (random 2) 0) r1 r2))))

;;; problem 12			       
(define empty-summary (list (list 0 0 0)
			    (list 0 0 0)
			    (list 0 0 0)))
(define (make-history-summary hist0 hist1 hist2)
  (define (helper h0 h1 h2 ccc ccd cct cdc cdd cdt ddc ddd ddt)
    (if (empty-history? h0)
	(list (list ccc ccd cct) (list cdc cdd cdt) (list ddc ddd ddt))
	(let ((r0 (most-recent-play h0))  (hh0 (rest-of-plays h0)) 
	      (r1 (most-recent-play h1))  (hh1 (rest-of-plays h1)) 
	      (r2 (most-recent-play h2))  (hh2 (rest-of-plays h2)))
	  (cond ((and (string=? r1 "d") (string=? r2 "d"))
		 (if (string=? r0 "c")
		     (helper hh0 hh1 hh2 ccc ccd cct cdc cdd cdt
			     (+ 1 ddc) ddd (+ 1 ddt))
		     (helper hh0 hh1 hh2 ccc ccd cct cdc cdd cdt
			     ddc (+ 1 ddd) (+ 1 ddt))))
		((and (string=? r1 "c") (string=? r2 "c"))
		 (if (string=? r0 "c")
		     (helper hh0 hh1 hh2 (+ 1 ccc) ccd (+ 1 cct) 
			     cdc cdd cdt ddc ddd ddt)
		     (helper hh0 hh1 hh2 ccc (+ 1 ccd) (+ 1 cct)
			     cdc cdd cdt ddc ddd ddt)))
		(else (if (string=? r0 "c")
			  (helper hh0 hh1 hh2 ccc ccd cct
				  (+ 1 cdc) cdd (+ 1 cdt) ddc ddd ddt)
			  (helper hh0 hh1 hh2 ccc ccd cct
				  cdc (+ 1 cdd) (+ 1 cdt) 
				  ddc ddd ddt)))))))
  (if (or (empty-history? hist0) 
	  (empty-history? (most-recent-play hist0)))
      empty-summary
      (helper (rest-of-plays hist0)
	      (rest-of-plays hist1)
	      (rest-of-plays hist2)  0 0 0 0 0 0 0 0 0)))

(define cooperate-cooperate car)
(define defect-cooperate cadr)
(define cooperate-defect cadr)
(define defect-defect caddr)
(define cooperate car)
(define defect cadr)
(define total caddr)

(define summary (make-history-summary
		 (list "c" "c" "d" "d" "c" "d" "c" "c")
		 (list "c" "c" "c" "d" "d" "c" "d" "c")
		 (list "c" "c" "d" "d" "d" "c" "c" "c")))
;;; problem 13
(define (get-probability-of-c summary)
  (let ((tcc (total (cooperate-cooperate summary)))
	(tdc (total (defect-cooperate summary)))
	(tdd (total (defect-defect summary)))
	(ccc (cooperate (cooperate-cooperate summary)))
	(cdc (cooperate (defect-cooperate summary)))
	(cdd (cooperate (defect-defect summary))))
    (let ((cc (if (= 0 tcc) (list) (/ ccc tcc)))
	  (dc (if (= 0 tdc) (list) (/ cdc tdc)))
	  (dd (if (= 0 tdd) (list) (/ cdd tdd))))
      (list cc dc dd))))

; test
(define summary (make-history-summary
		 (list "c" "c" "c" "c")
		 (list "d" "d" "d" "c")
		 (list "d" "d" "c" "c")))
(get-probability-of-c summary)
;Value: (1 1 1)
(define new-summary (make-history-summary
		     (list "c" "c" "c" "d" "c")
		     (list "d" "c" "d" "d" "c")
		     (list "d" "c" "c" "c" "c")))
(get-probability-of-c new-summary)
;Value: (0.5 1 ())

;;; problem 14

;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
(define (test-entry expected-values actual-values) 
   (cond ((null? expected-values) (null? actual-values)) 
         ((null? actual-values) #f) 
         ((or (not (car expected-values)) 
              (not (car actual-values)) 
              (= (car expected-values) (car actual-values))) 
          (test-entry (cdr expected-values) (cdr actual-values))) 
         (else #f))) 

(define (my-test-entry v1 v2)
  (cond ((null? v1) (null? v2))
	((null? v2) #f)
	(else (let ((a1 (car v1))
		    (a2 (car v2)))
		(cond ((null? a1) (null? a2))
		      ((null? a2) #f)
		      (else (if (= a1 a2)
				(my-test-entry (cdr v1) (cdr v2))
				#f)))))))


(test-entry (list 1 1 1) (list 1 1 1))
(test-entry (list 1 1 1) (list 1 1 0))
(test-entry (list 1 1 (list)) (list 1 1 (list)))
(test-entry (list 1 1 1) (list 1 1 (list)))

(my-test-entry (list 1 1 1) (list 1 1 1))
(my-test-entry (list 1 1 1) (list 1 1 0))
(my-test-entry (list 1 1 (list)) (list 1 1 (list)))
(my-test-entry (list 1 1 1) (list 1 1 (list)))

(define (is-he-a-fool? hist0 hist1 hist2) 
   (my-test-entry (list 1 1 1) 
               (get-probability-of-c 
                (make-history-summary hist0 hist1 hist2))))

(define (could-he-be-a-fool? hist0 hist1 hist2)
  (my-test-entry (list 1 1 1)
              (map (lambda (elt) 
                      (cond ((null? elt) 1)
                            ((= elt 1) 1)  
                            (else 0)))
                   (get-probability-of-c (make-history-summary hist0 
                                                               hist1
                                                               hist2)))))

(define (is-soft-eye-for-eye? hist0 hist1 hist2)
  (my-test-entry (list 1 1 0)
	      (get-probability-of-c (make-history-summary hist0
							  hist1
							  hist2))))
(define (could-be-soft-eye-for-eye? h0 h1 h2)
  (define d (list 0 1 1 0))
  (define (map-to elt)
    (set! d (cdr d))
    (if (null? elt) (car d) elt))
  (my-test-entry (list 1 1 0)
	      (map map-to 
		   (get-probability-of-c (make-history-summary h0
							       h1
							       h2)))))
      
(is-soft-eye-for-eye? (list "c" "c" "c" "c")
		      (list "d" "d" "d" "c")
		      (list "d" "d" "c" "c"))
(is-soft-eye-for-eye? (list "c" "c" "c" "c" "c")
		      (list "d" "c" "d" "d" "c")
		      (list "d" "c" "c" "c" "c"))
(is-soft-eye-for-eye? (list "c" "c" "d" "d" "c" "d" "c" "c")
		      (list "c" "c" "c" "d" "d" "c" "d" "c")
		      (list "c" "c" "d" "d" "d" "c" "c" "c"))
(is-soft-eye-for-eye? (list "c" "c" "c" "d" "d" "c" "c" "c")
		      (list "c" "c" "c" "d" "d" "c" "d" "c")
		      (list "c" "c" "d" "d" "d" "c" "c" "c"))


(could-be-soft-eye-for-eye? (list "c" "c" "c" "c")
			    (list "d" "d" "d" "c")
			    (list "d" "d" "c" "c"))
(could-be-soft-eye-for-eye? (list "c" "c" "c" "c" "c")
			    (list "d" "c" "d" "d" "c")
			    (list "d" "c" "c" "c" "c"))
(could-be-soft-eye-for-eye? (list "c" "c" "d" "d" "c" "d" "c" "c")
			    (list "c" "c" "c" "d" "d" "c" "d" "c")
			    (list "c" "c" "d" "d" "d" "c" "c" "c"))
(could-be-soft-eye-for-eye? (list "c" "c" "c" "d" "d" "c" "c" "c")
			    (list "c" "c" "c" "d" "d" "c" "d" "c")
			    (list "c" "c" "d" "d" "d" "c" "c" "c"))


(define (dont-tolerate-fools my-hist hist1 hist2)
  (if (< (length my-hist) 10)
      "c"
      (if (and (could-he-be-a-fool? hist1 my-hist hist2)
	       (could-he-be-a-fool? hist2 my-hist hist1))
	  "d"
	  "d")))
(dont-tolerate-fools (list "c" "c" "c" "c")
		      (list "d" "d" "d" "c")
		      (list "d" "d" "c" "c"))
(dont-tolerate-fools (list "c" "c" "c" "d" "c")
		      (list "d" "c" "d" "d" "c")
		      (list "d" "c" "c" "c" "c"))
(dont-tolerate-fools (list "c" "c" "d" "d" "c" "d" "c" "c")
		      (list "c" "c" "c" "d" "d" "c" "d" "c")
		      (list "c" "c" "d" "d" "d" "c" "c" "c"))
(dont-tolerate-fools (list "c" "c" "c" "d" "d" "c" "c" "c" "c" "c")
		     (list "c" "c" "c" "c" "c" "c" "c" "c" "c" "c")
		     (list "c" "c" "c" "c" "c" "c" "c" "c" "c" "c"))
