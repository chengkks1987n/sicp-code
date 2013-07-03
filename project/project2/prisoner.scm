;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (else (let ((result0 (strat0 history0 history1))
		      (result1 (strat1 history1 history0)))
		  (play-loop-iter strat0 strat1 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
	   (list score0 score1))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

;; note that you will need to write extract-entry

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN  my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 1
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
  
;; test cases
(define a-play (make-play "c" "d"))
(extract-entry a-play *game-association-list*)
;Value 14: (("c" "d") (0 5))
(define a-play (make-play "c" "e"))
;(extract-entry a-play *game-association-list*)
;cannot find game in the list
(define a-play (make-play "c" "c"))
(extract-entry a-play *game-association-list*)
;Value 15: (("c" "c") (3 3))
(define a-play (make-play "c"))
;(extract-entry a-play *game-association-list*)
;cannot find game in the list

;; problem 2
(define players 
  (list NASTY PATSY SPASTIC EYE-FOR-EYE EGALITARIAN))

(define (play-game player player-list)
  (if (null? player-list)
      (display "done")
      (play-loop player (car player-list)))
  (cond ((not (null? player-list))
	 (play-game player (cdr player-list)))))

(define (all-play-game pl1 pl2)
  (if (null? pl1)
      (display "'all-done")
      (play-game (car pl1) pl2))
  (cond ((not (null? pl1)) 
	 (all-play-game (cdr pl1) pl2))))
(all-play-game players players)

;;; problem 3
;; -the order of growth is theta(n) in both time and space 
;;  for the older one. it is recurcive.
;; -the order of growth is theta(n) in time; theta(1) in spacs
;;  for the newer
;; -the newer is faster. it takes n comparisions while the older
;;  takes 2n comparisions;  and we know recursion is faster then
;;  iterator 

;;; problem 4
(define (eye-for-two-eye my-history other-history)
  (if (and (not (empty-history? other-history))
	   (string=? (most-recent-play other-history) "d"))
      (let ((rests (rest-of-plays other-history)))
	(if (and (not (empty-history? rests))
		 (string=? (most-recent-play rests) "d"))
	    "d" "c"))
      "c"))

(define players 
  (list NASTY PATSY SPASTIC EYE-FOR-EYE EGALITARIAN eye-for-two-eye))
(play-game eye-for-two-eye players)
	
;;; problem 5      
(define (make-eye-for-n-eye n)
  (define (helper n history)
    (if (= 0 n)
	"d"
	(if (null? history)
	    "c"
	    (if (string=? "d" (car history))
		(helper (- n 1) (cdr history))
		"c"))))
  (lambda (my-history other-history)
    (helper n other-history)))
(define eye-for2eye (make-eye-for-n-eye 2))
(define eye-for1eye (make-eye-for-n-eye 1))

(play-game eye-for2eye players)
(play-game eye-for-two-eye players)
(play-game eye-for-eye players)
(play-game eye-for1eye players)

;;; problem 6

(define (make-rotating-stategy stat0 stat1 freq0 freq1)
  (define r -1)
  (define f (+ freq0 freq1))
  (lambda (my-history other-history)
    (set! r (remainder (+ 1 r) f))
    (if (< r freq0) 
	(stat0 my-history other-history)
	(stat1 my-history other-history))))

(define rs (make-rotating-stategy nasty PATSY 1 1))
(rs "a" "a")  ;; eval some times
(define ps (make-rotating-stategy PATSY nasty 1 1))
(ps "a" "a")  ;; eval some times

;;; problem 7
(define (make-higher-order-spastic list-stat)
  (define current (list))
  (lambda (my-history other-history)
    (if (null? current)
	(set! current list-stat)
	(set! current (cdr current)))
    (cond ((null? current) (set! current list-stat)))
    ((car current) my-history other-history)))

(define npp (make-higher-order-spastic (list nasty patsy patsy)))
(npp "a" "b") ; eval some times
(define pn (make-higher-order-spastic (list patsy nasty)))
(pn "a" "b") ; eval some times

;;; problem 8
(define (gentle stat gentleness-factor)
  (lambda (my-history other-history)
    (if (string=? "c" (stat my-history other-history))
	"c"
	(let ((r (random 1.0)))
	  (if (< r gentleness-factor) "c" "d")))))

(define neg-n (gentle nasty 1))
(neg-n 'a 'b)
(define neg-n (gentle nasty 0))
(neg-n 'a 'b)
(define neg-n (gentle nasty 0.5))
(neg-n 'a 'b)



;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;	    

;(define *game-association-list*
;  (list (list (list "c" "c" "c") (list 4 4 4))
;        (list (list "c" "c" "d") (list 2 2 5))
;        (list (list "c" "d" "c") (list 2 5 2))
;        (list (list "d" "c" "c") (list 5 2 2))
;        (list (list "c" "d" "d") (list 0 3 3))
;        (list (list "d" "c" "d") (list 3 0 3))
;        (list (list "d" "d" "c") (list 3 3 0))
;        (list (list "d" "d" "d") (list 1 1 1))))


;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
;(define (test-entry expected-values actual-values) 
;   (cond ((null? expected-values) (null? actual-values)) 
;         ((null? actual-values) #f) 
;         ((or (not (car expected-values)) 
;              (not (car actual-values)) 
;              (= (car expected-values) (car actual-values))) 
;          (test-entry (cdr expected-values) (cdr actual-values))) 
;         (else #f))) 
;
;(define (is-he-a-fool? hist0 hist1 hist2) 
;   (test-entry (list 1 1 1) 
;               (get-probability-of-c 
;                (make-history-summary hist0 hist1 hist2))))
;
;(define (could-he-be-a-fool? hist0 hist1 hist2)
;  (test-entry (list 1 1 1)
;              (map (lambda (elt) 
;                      (cond ((null? elt) 1)
;                            ((= elt 1) 1)  
;                            (else 0)))
;                   (get-probability-of-c (make-history-summary hist0 
;                                                               hist1
;                                                               hist2)))))

