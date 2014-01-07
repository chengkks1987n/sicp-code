;;; computer exercise 6 tests
;; give the command and the outpu result after the (setup 'cheky)

(load "objsys.scm")
(load "objtypes.scm")
(load "setup.scm")
(setup 'cheky)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test: boil-spell and slug spell should only apply to a person object.
(ask me'look-around)

You are in grendels-den 
You are not holding anything. 
You see stuff in the room: a-ring-of-obfuscation boil-spell 
There are no other people around you. 
The exits are in directions: up 
;Value: ok

1 ]=> (ask (thing-named 'boil-spell) 'use me me)

At grendels-den cheky grows boils on their nose 
;Value: message-displayed

1 ]=> (ask (thing-named 'boil-spell) 'use me (thing-named 'a-ring-of-obfuscation))

At grendels-den nothing happend. a-ring-of-obfuscation is not a person 
;Value: message-displayed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test: Wind-ofdoom

(load "objsys.scm")
(load "objtypes.scm")
(load "setup.scm")
(setup 'cheky)

(ask me 'look-around)

You are in eecs-hq 
You are not holding anything. 
You see stuff in the room: a-ring-of-obfuscation wind-of-doom 
You see other people: grendel 
The exits are in directions: up west down 
;Value: ok

1 ]=> (ask (thing-named 'wind-of-doom) 'use me (ask me 'location))

nothing happend. eecs-hq is neither a thing or a person 
;Value: message-displayed

1 ]=> (ask (thing-named 'wind-of-doom) 'use me (thing-named 'a-ring-of-obfuscation))

At eecs-hq a-ring-of-obfuscation is destroyed by cheky 
;Value: done

1 ]=> (ask me 'look-around)

You are in eecs-hq 
You are not holding anything. 
You see stuff in the room: wind-of-doom 
You see other people: grendel 
The exits are in directions: up west down 
;Value: ok

1 ]=> (ask (thing-named 'wind-of-doom) 'use me (thing-named 'grendel))

At eecs-hq grendel suffers 2 damage 
At eecs-hq grendel says -- Ouch! 2 hits is more than I want! 
;Value: 1

1 ]=> (ask (thing-named 'wind-of-doom) 'use me (thing-named 'grendel))

At eecs-hq grendel suffers 1 damage 
At eecs-hq grendel says -- Ouch! 1 hits is more than I want! 
At eecs-hq grendel says -- SHREEEEK!  I, uh, suddenly feel very faint... 
An earth-shattering, soul-piercing scream is heard... 
;Value: 0

1 ]=> (ask me 'look-around)

You are in eecs-hq 
You are not holding anything. 
You see stuff in the room: wind-of-doom 
There are no other people around you. 
The exits are in directions: up west down 
;Value: ok
