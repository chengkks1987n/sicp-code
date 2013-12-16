
(define (instance-handler? objs)
  (cond ((null? objs) #t)
	((or (handler? (car objs)) (instance? (car objs))) 
	 (instance-handler? (cdr objs)))
	(else #f)))

(load "objsys.scm")
(load "objtypes.scm")
(load "setup.scm")

;; warmup exercise 6
(setup 'cheky)
(ask me 'look-around)
;You are in great-court 
;You are not holding anything. 
;You see stuff in the room: slug-spell flag-pole lovely-trees 
;There are no other people around you. 
;The exits are in directions: up west north 
;;Value: ok
(ask me 'take (thing-named 'slug-spell))
;At great-court cheky says -- I take slug-spell from great-court 
;;Value 14: (instance #[compound-procedure 15 handler])
(ask me 'look-around)
;You are in great-court 
;You are holding: slug-spell 
;You see stuff in the room: flag-pole lovely-trees 
;There are no other people around you. 
;The exits are in directions: up west north 
;;Value: ok
(ask me 'take (thing-named 'lovely-trees))
;At great-court cheky says -- I try but cannot take lovely-trees 
;;Value: #f
(ask me 'go 'up)
;cheky moves from great-court to graduation-stage 
;--- the-clock Tick 0 --- 
;ben-bitdiddle moves from 34-301 to edgerton-hall 
;alyssa-hacker moves from barker-library to 10-250 
;At 10-250 alyssa-hacker says -- I take boil-spell from 10-250 
;course-6-frosh moves from barker-library to 10-250 
;At 10-250 course-6-frosh says -- Hi alyssa-hacker 
;course-6-frosh moves from 10-250 to lobby-10 
;lambda-man moves from baker to bexley 
;At bexley lambda-man says -- I take boil-spell from bexley 
;dr-evil moves from lobby-7 to student-center 
;At student-center dr-evil says -- Hi mr-bigglesworth 
;dr-evil moves from student-center to bexley 
;At bexley dr-evil says -- Hi lambda-man 
;At bexley dr-evil says -- I'll let you off this once... 
;mr-bigglesworth moves from student-center to bexley 
;At bexley mr-bigglesworth says -- Hi dr-evil lambda-man 
;At bexley mr-bigglesworth says -- I'll let you off this once... 
;grendel moves from stata-center to stata-center 
;grendel moves from stata-center to stata-center 
;At stata-center grendel says -- I take sicp from stata-center 
;At stata-center grendel 's belly rumbles 
;registrar moves from 34-301 to stata-center 
;At stata-center registrar says -- Hi grendel 
;registrar moves from stata-center to stata-center 
;At stata-center registrar says -- Hi grendel 
;At stata-center registrar takes a bite out of grendel 
;At stata-center grendel says -- Ouch! 3 hits is more than I want! 
;At stata-center grendel says -- SHREEEEK!  I, uh, suddenly feel very faint... 
;At stata-center grendel says -- I lose sicp 
;At stata-center grendel says -- Yaaaah! I am upset! 
;An earth-shattering, soul-piercing scream is heard... 
;;Value: #t
(ask me 'look-around)
;You are in graduation-stage 
;You are holding: slug-spell 
;You see stuff in the room: boil-spell diploma 
;There are no other people around you. 
;The exits are in directions: down 
;;Value: ok
(ask me 'drop (thing-named 'slug-spell))
;At graduation-stage cheky says -- I drop slug-spell at graduation-stage 
;;Value 16: (instance #[compound-procedure 17 handler])
(ask me 'look-around)
;You are in graduation-stage 
;You are not holding anything. 
;You see stuff in the room: slug-spell boil-spell diploma 
;There are no other people around you. 
;The exits are in directions: down 
;;Value: ok
(ask me 'take (thing-named 'slug-spell))
;At graduation-stage cheky says -- I take slug-spell from graduation-stage;Value 14: (instance #[compound-procedure 15 handler])
(ask me 'take (thing-named 'boil-spell))
;At graduation-stage cheky says -- I take boil-spell from graduation-stage 
;Value 18: (instance #[compound-procedure 19 handler])
(ask (thing-named 'slug-spell) 'use me me)
;At graduation-stage A slug comes out of cheky 's mouth. 
;;Value 20: (instance #[compound-procedure 21 handler])
(ask me 'look-around)
;You are in graduation-stage 
;You are holding: boil-spell slug-spell 
;You see stuff in the room: slug diploma 
;There are no other people around you. 
;The exits are in directions: down 
;;Value: ok
(ask me 'suffer 4 me)
;At graduation-stage cheky says -- Ouch! 4 hits is more than I want! 
;At graduation-stage cheky says -- I am slain! 
;At graduation-stage cheky says -- I lose boil-spell 
;At graduation-stage cheky says -- Yaaaah! I am upset! 
;At graduation-stage cheky says -- I lose slug-spell 
;At graduation-stage cheky says -- Yaaaah! I am upset! 
;An earth-shattering, soul-piercing scream is heard... 
;;Value: -1
(ask me 'look-around)
;You are in graduation-stage 
;You are not holding anything. 
;You see stuff in the room: slug-spell boil-spell slug diploma 
;There are no other people around you. 
;The exits are in directions: down 
;;Value: ok

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; computer exercise 1
(setup 'cheky)
;; 1. thing-part of the avator.
(show me)
; INSTANCE (instance #[compound-procedure 22 handler])
; TYPE: (avatar person mobile-thing thing named-object root container)
; HANDLER: #[compound-procedure 22 handler]
; TYPE: avatar
;(methods (look-around #[compound-procedure 25])
;         (go #[compound-procedure 24])
;         (die #[compound-procedure 23]))
;  Parent frame: #[environment 26]
;  person-part:  #[compound-procedure 27 handler]
;    Parent frame: global-environment
;    self:         (instance #[compound-procedure 22 handler])
;    name:         cheky
;    birthplace:   (instance #[compound-procedure 28 handler])
;;Value: instance
(show #@27)   ;person-part of me
;  HANDLER: #[compound-procedure 27 handler]
; TYPE: person
;(methods (strength #[compound-procedure 33])
;         (health #[compound-procedure 32])
;         (say #[compound-procedure 31])
;         (have-fit #[compound-procedure 30])
;         (people-around #[compound-procedure 29])
;         ...)
;  Parent frame:      #[environment 34]
;  mobile-thing-part: #[compound-procedure 35 handler]
;  container-part:    #[compound-procedure 36 handler]
;  health:            3
;  strength:          1
;    Parent frame: global-environment
;    self:         (instance #[compound-procedure 22 handler])
;    name:         cheky
;    birthplace:   (instance #[compound-procedure 28 handler])
;;Value: handler
(show #@35) ; mobile-thing-part of person-part
;  HANDLER: #[compound-procedure 35 handler]
; TYPE: mobile-thing
;(methods (location #[compound-procedure 41])
;         (change-location #[compound-procedure 40])
;         (enter-room #[compound-procedure 39])
;         (leave-room #[compound-procedure 38])
;         (creation-site #[compound-procedure 37]))
;  Parent frame: #[environment 42]
;  thing-part:   #[compound-procedure 43 handler]
;    Parent frame: global-environment
;    self:         (instance #[compound-procedure 22 handler])
;    name:         cheky
;    location:     (instance #[compound-procedure 28 handler])
;;Value: handler
(show #@43) ; thing-part of mobile-thing-part
;  HANDLER: #[compound-procedure 43 handler]
; TYPE: thing
;(methods (install #[compound-procedure 47])
;         (location #[compound-procedure 46])
;         (destroy #[compound-procedure 45])
;         (emit #[compound-procedure 44]))
;  Parent frame: #[environment 48]
;  named-part:   #[compound-procedure 49 handler]
;    Parent frame: global-environment
;    self:         (instance #[compound-procedure 22 handler])
;    name:         cheky
;    location:     (instance #[compound-procedure 28 handler])
;;Value: handler

;; 2) the container-part of the place in which the avatar resides       
(show #@28) ; location of thing-part
;  HANDLER: #[compound-procedure 28 handler]
; TYPE: place
;(methods (exits #[compound-procedure 52])
;         (exit-towards #[compound-procedure 51])
;         (add-exit #[compound-procedure 50]))
;  Parent frame:   #[environment 53]
;  named-part:     #[compound-procedure 54 handler]
;  container-part: #[compound-procedure 55 handler]
;  exits:          ((instance #[compound-procedure 57 handler])
;                 (instance #[compound-procedure 56 handler]))
;    Parent frame: global-environment
;    self:         (instance #[compound-procedure 28 handler])
;    name:         lobby-7
;;Value: handler
(show #@55)
;  HANDLER: #[compound-procedure 55 handler]
; TYPE: container
;(methods (things #[compound-procedure 61])
;         (have-thing? #[compound-procedure 60])
;         (add-thing #[compound-procedure 59])
;         (del-thing #[compound-procedure 58]))
;  Parent frame: #[environment 62]
;  root-part:    #[compound-procedure 63 handler]
;  things:       ((instance #[compound-procedure 22 handler])
;               (instance #[compound-procedure 64 handler]))
;    Parent frame: global-environment
;    self:         (instance #[compound-procedure 28 handler])
;;Value: handler
;; 3) move, see the difference betwwen location and birthplace.
(ask me 'look-around)
;You are in lobby-7 
;You are not holding anything. 
;You see stuff in the room: slug-spell 
;There are no other people around you. 
;The exits are in directions: west east 
;;Value: ok
(ask me 'go 'east)
(show #@35) ; mobile-thing-part of person-part in me. th loaction is different
; HANDLER: #[compound-procedure 35 handler]
; TYPE: mobile-thing
;(methods (location #[compound-procedure 41])
;         (change-location #[compound-procedure 40])
;         (enter-room #[compound-procedure 39])
;         (leave-room #[compound-procedure 38])
;         (creation-site #[compound-procedure 37]))
;  Parent frame: #[environment 42]
;  thing-part:   #[compound-procedure 43 handler]
;    Parent frame: global-environment
;    self:         (instance #[compound-procedure 22 handler])
;    name:         cheky
;    location:     (instance #[compound-procedure 65 handler])
;;Value: handler
(show #@65) ; location of mobile-thing-part
; HANDLER: #[compound-procedure 65 handler]
; TYPE: place
;(methods (exits #[compound-procedure 68])
;         (exit-towards #[compound-procedure 67])
;         (add-exit #[compound-procedure 66]))
;  Parent frame:   #[environment 69]
;  named-part:     #[compound-procedure 70 handler]
;  container-part: #[compound-procedure 71 handler]
;  exits:          ((instance #[compound-procedure 76 handler])
;                 (instance #[compound-procedure 75 handler])
;                 (instance #[compound-procedure 74 handler])
;                 (instance #[compound-procedure 73 handler])
;                 (instance #[compound-procedure 72 handler]))
;    Parent frame: global-environment
;    self:         (instance #[compound-procedure 65 handler])
;    name:         lobby-10
;;Value: handler
(show #@43) ; same as before
(show #@28) ; same as before

;; 4) is self changed?
; self never change. it always point to me.

