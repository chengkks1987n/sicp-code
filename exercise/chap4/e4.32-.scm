;;; exercise 4.32
; in chapter 3, the car of the stream is not lazy, but in this section they are 
; both lazy.
; for an object its car part is also infinite such as an infinite tree,
; the 'lazier' lazy is the right choice.

(load "meval.scm")
(load "environment.scm")
(load "syntax.scm")
(load "leval.scm")
;;; exercise 4.33

