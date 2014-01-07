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

;;;; computer exercise 7 test
(load "objtypes.scm")
(load "setup.scm")
(setup 'cheky)


At baker ben-bitdiddle says -- I take shortest-wand from baker 
At 6001-lab alyssa-hacker says -- I take shortest-wand from 6001-lab 
At eecs-ug-office course-6-frosh says -- I take shortest-wand from eecs-ug-office 
At barker-library lambda-man says -- I take shortest-wand from barker-library 
;Value: ready

1 ]=> (run-clock 1)

--- the-clock Tick 0 --- 
ben-bitdiddle moves from baker to bexley 
At bexley ben-bitdiddle says -- Hi grendel 
ben-bitdiddle moves from bexley to student-center 
At student-center ben-bitdiddle wave the shortest-wand toward slug-spell , but nothing happened. 
alyssa-hacker moves from 6001-lab to eecs-hq 
At eecs-hq alyssa-hacker says -- Hi cheky 
alyssa-hacker moves from eecs-hq to eecs-ug-office 
At eecs-ug-office alyssa-hacker says -- Hi course-6-frosh 
At eecs-ug-office alyssa-hacker says -- I take a-ring-of-obfuscation from eecs-ug-office 
At eecs-ug-office alyssa-hacker wave the shortest-wand toward course-6-frosh , but nothing happened. 
course-6-frosh moves from eecs-ug-office to eecs-hq 
At eecs-hq course-6-frosh says -- Hi cheky 
course-6-frosh moves from eecs-hq to 6001-lab 
course-6-frosh moves from 6001-lab to eecs-hq 
At eecs-hq course-6-frosh says -- Hi cheky 
At eecs-hq course-6-frosh says -- I take slug-spell from eecs-hq 
At eecs-hq course-6-frosh wave the shortest-wand toward cheky , incant dagnabbit ekaterin 
At eecs-hq A slug comes out of cheky 's mouth. 
lambda-man moves from barker-library to 10-250 
lambda-man moves from 10-250 to lobby-10 
lambda-man moves from lobby-10 to 10-250 
At 10-250 lambda-man says -- I take a-ring-of-obfuscation from 10-250 
At 10-250 lambda-man wave the shortest-wand toward course-6-frosh , but nothing happened. 
dr-evil moves from barker-library to 10-250 
At 10-250 dr-evil says -- I'll let you off this once... 
mr-bigglesworth moves from baker to bexley 
At bexley mr-bigglesworth says -- Hi grendel 
At bexley mr-bigglesworth says -- I take wind-of-doom from bexley 
At bexley mr-bigglesworth says -- I'll let you off this once... 
grendel moves from bexley to student-center 
At student-center grendel says -- Hi ben-bitdiddle 
At student-center grendel takes a bite out of ben-bitdiddle 
At student-center ben-bitdiddle says -- Ouch! 1 hits is more than I want! 
registrar moves from barker-library to 10-250 
At 10-250 registrar says -- Hi dr-evil 
At 10-250 registrar takes a bite out of dr-evil 
At 10-250 dr-evil says -- Ouch! 3 hits is more than I want! 
At 10-250 dr-evil says -- SHREEEEK!  I, uh, suddenly feel very faint... 
An earth-shattering, soul-piercing scream is heard... 
;Value: done

1 ]=> (run-clock 1)

--- the-clock Tick 1 --- 
ben-bitdiddle moves from student-center to lobby-7 
At lobby-7 ben-bitdiddle says -- I take wind-of-doom from lobby-7 
At lobby-7 ben-bitdiddle wave the shortest-wand toward slug , incant huuuuu huuuuu 
At eecs-hq slug is destroyed by ben-bitdiddle 
alyssa-hacker moves from eecs-ug-office to eecs-hq 
At eecs-hq alyssa-hacker says -- Hi course-6-frosh cheky 
alyssa-hacker moves from eecs-hq to 6001-lab 
At 6001-lab alyssa-hacker says -- I take a-ring-of-obfuscation from 6001-lab 
At 6001-lab alyssa-hacker wave the shortest-wand toward a-ring-of-obfuscation , but nothing happened. 
course-6-frosh moves from eecs-hq to 34-301 
course-6-frosh moves from 34-301 to edgerton-hall 
At edgerton-hall course-6-frosh says -- I take a-ring-of-obfuscation from edgerton-hall 
At edgerton-hall course-6-frosh wave the shortest-wand toward a-ring-of-obfuscation , incant dagnabbit ekaterin 
At eecs-hq nothing happend. a-ring-of-obfuscation is not a person 
lambda-man moves from 10-250 to barker-library 
lambda-man moves from barker-library to 10-250 
At 10-250 lambda-man says -- Hi registrar 
At 10-250 lambda-man wave the shortest-wand toward registrar , but nothing happened. 
mr-bigglesworth moves from bexley to student-center 
At student-center mr-bigglesworth says -- Hi grendel 
At student-center mr-bigglesworth says -- I'll let you off this once... 
grendel moves from student-center to bexley 
registrar moves from 10-250 to barker-library 
At barker-library registrar 's belly rumbles 
;Value: done

