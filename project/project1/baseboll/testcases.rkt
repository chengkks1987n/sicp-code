#lang racket
(require "baseball.rkt")

(position 0 0 0 0)
(position 0 0 20 0)
(position 0 5 10 10)
(position 2 2 2 2)
(position 5 5 5 5)

(root1 5 3 6)
(root1 1 2 1)
(root2 1 2 1)

(time-to-impact 1 45)

(travel-distance-simple 1 45 0)
(travel-distance-simple 1 45 45)
(travel-distance-simple 1 45 90)

(find-best-angle 45 1)

(display "travel-distance e v angle")
(newline)
(travel-distance 1 45 0)
(travel-distance 1 45 45)
(travel-distance 1 45 90)
(travel-distance 1 40 45)
(travel-distance 1 35 45)
(travel-distance 0 35 45)
(travel-distance 0.00001 35 45)
(travel-distance 1 45 -17.17)

(display "best-angle-for-distance e v dst")
(newline)
(define a1 (best-angle-for-distance 1 45 36))
a1
(time-to-impact (* 45 (sin a1)) 1)
(define a2 (best-angle-for-distance 1 35 36))
a2
(time-to-impact (* 35 (sin a2)) 1)
(define a3 (best-angle-for-distance 1 55 36))
a3
(time-to-impact (* 55 (sin a3)) 1)
(best-angle-for-distance 1 45 3.4)
(best-angle-for-distance 1 45 18)
(best-angle-for-distance 1 45 30)
(best-angle-for-distance 1 45 60)
(best-angle-for-distance 1 45 90)
