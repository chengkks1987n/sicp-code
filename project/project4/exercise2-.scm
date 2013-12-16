(load "objsys.scm")
(load "objtypes.scm")
(load "setup.scm")

;;; computer exercise 2
;; see file 'objtypes.scm' procedure person - HAS-A and HAS-A-THING-NAMED
;test  HAS-A 
(setup 'cheky)
(ask me 'HAS-A 'spell)
;Value: ()
(ask me 'has-a-thing-named 'flag)
;Value: ()
(ask me 'has-a-thing-named 'boil-spell)
;Value: ()
(ask me 'has-a-thing-named 'slug-spell)
;Value: ()
(ask  me 'look-around)
;You are in great-court 
;You are not holding anything. 
;You see stuff in the room: boil-spell flag-pole lovely-trees 
;There are no other people around you. 
;The exits are in directions: up west north 
;;Value: ok

(ask me 'take (thing-named 'boil-spell))
;At great-court cheky says -- I take boil-spell from great-court 
;;Value 20: (instance #[compound-procedure 21 handler])
(ask me 'has-a 'spell)
;Value 22: ((instance #[compound-procedure 21 handler]))
(ask me 'has-a 'book)
;Value: ()
(ask me 'has-a-thing-named 'spell)
;Value: ()

(ask me 'take (thing-named 'flag-pole))
At great-court cheky says -- I try but cannot take flag-pole 
;Value: #f
(ask me 'has-a-thing-named 'flag-pole)
;Value: ()

(ask me 'drop (thing-named 'boil-spell))
;At great-court cheky says -- I drop boil-spell at great-court 
;;Value 23: (instance #[compound-procedure 24 handler])
(ask me 'HAS-A 'spell)
;Value: ()
(ask me 'has-a-thing-named 'flag)
;Value: ()
(ask me 'has-a-thing-named 'boil-spell)
;Value: ()
(ask me 'has-a-thing-named 'slug-spell)
;Value: ()

;;; computer exercise 3
;; see file 'objtypes.scm' procedure avatar FEEL-THE-FORCE
(load "objsys.scm")
(load "objtypes.scm")
(load "setup.scm")
(setup 'cheky)
(ask me 'feel-the-force)
;registrar at grendels-den 
;lambda-man at barker-library 
;ben-bitdiddle at edgerton-hall 
;alyssa-hacker at building-13 
;course-6-frosh at great-court 
;grendel at bexley 
;mr-bigglesworth at graduation-stage 
;dr-evil at graduation-stage 
;;Unspecified return value
