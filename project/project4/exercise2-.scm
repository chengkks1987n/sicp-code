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


;;; computer exercise 3
(ask me 'feel-the-force)
;
;registrar at 10-250 
;course-6-frosh at lobby-10 
;mr-bigglesworth at barker-library 
;grendel at edgerton-hall 
;ben-bitdiddle at edgerton-hall 
;lambda-man at great-court 
;alyssa-hacker at student-center 
;dr-evil at bexley 

(run-clock 1)
;
;--- the-clock Tick 0 --- 
;ben-bitdiddle moves from edgerton-hall to building-13 
;At building-13 ben-bitdiddle says -- I take thing-of-obfuscation from building-13 
;alyssa-hacker moves from student-center to bexley 
;At bexley alyssa-hacker says -- Hi dr-evil 
;course-6-frosh moves from lobby-10 to lobby-7 
;lambda-man moves from great-court to graduation-stage 
;At graduation-stage lambda-man says -- I take thing-of-obfuscation from graduation-stage 
;dr-evil moves from bexley to student-center 
;At student-center dr-evil says -- I'll let you off this once... 
;mr-bigglesworth moves from barker-library to 10-250 
;At 10-250 mr-bigglesworth says -- Hi cheky registrar 
;mr-bigglesworth moves from 10-250 to lobby-10 
;At lobby-10 mr-bigglesworth says -- What are you doing still up? Everyone back to their rooms! 
;grendel moves from edgerton-hall to legal-seafood 
;At legal-seafood grendel 's belly rumbles 
;registrar moves from 10-250 to lobby-10 
;At lobby-10 registrar says -- Hi mr-bigglesworth 
;;Value: done

(ask me 'feel-the-force)
;
;registrar at lobby-10 
;mr-bigglesworth at lobby-10 
;course-6-frosh at lobby-7 
;dr-evil at student-center 
;alyssa-hacker at bexley 
;grendel at legal-seafood 
;;Unspecified return value

;;;ck! explain:
;; pay attation to the difference of the two results of 
;  (ask me 'feel-the-force), one is before (run-clock 1), the other is 
;  after that. ben-bitdiddle and lambda-man are disappeared.
; in the result of (run-clock 1), ben-bitdiddle and lambda-man take the
;  thing-of-obfuscation, so they disappear in the second feel-the-force.
