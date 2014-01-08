;;; computer exercise 8 tests
;; give the command and the outpu result after the (setup 'cheky)

(load "objsys.scm")
(load "objtypes.scm")
(load "setup.scm")
(setup 'cheky)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

1 ]=> (run-clock 1)

--- the-clock Tick 0 --- 
ben-bitdiddle moves from lobby-7 to lobby-10 
At lobby-10 ben-bitdiddle says -- Hi grendel 
ben-bitdiddle moves from lobby-10 to 10-250 
At 10-250 ben-bitdiddle says -- Hi dr-evil 
At 10-250 ben-bitdiddle says -- I take slug-spell from 10-250 
At 10-250 ben-bitdiddle wave the shortest-wand toward dr-evil , incant dagnabbit ekaterin 
At 10-250 A slug comes out of dr-evil 's mouth. 
alyssa-hacker moves from great-court to legal-seafood 
At legal-seafood alyssa-hacker says -- I take slug-spell from legal-seafood 
At legal-seafood alyssa-hacker wave the shortest-wand toward sicp , incant dagnabbit ekaterin 
At stata-center nothing happend. sicp is not a person 
course-6-frosh moves from building-13 to lobby-10 
At lobby-10 course-6-frosh says -- Hi grendel 
At lobby-10 course-6-frosh wave the shortest-wand toward grendel , but nothing happened. 
lambda-man moves from eecs-ug-office to eecs-hq 
lambda-man moves from eecs-hq to 6001-lab 
At 6001-lab lambda-man says -- I take slug-spell from 6001-lab 
At 6001-lab lambda-man wave the shortest-wand toward slug-spell , incant dagnabbit ekaterin 
At stata-center nothing happend. slug-spell is not a person 
susan-hockfield moves from building-13 to lobby-10 
At lobby-10 susan-hockfield says -- Hi course-6-frosh grendel 
At lobby-10 susan-hockfield says -- I take a-ring-of-obfuscation from lobby-10 
At lobby-10 susan-hockfield wave the shortest-wand toward grendel , but nothing happened. 
At lobby-10 susan-hockfield teaches course-6-frosh the spell -- slug-spell 
eric-grimson moves from bexley to baker 
At baker eric-grimson wave the shortest-wand toward sicp , but nothing happened. 
dr-evil moves from 10-250 to barker-library 
dr-evil moves from barker-library to 10-250 
At 10-250 dr-evil says -- Hi ben-bitdiddle 
At 10-250 dr-evil says -- What are you doing still up? Everyone back to their rooms! 
At 10-250 ben-bitdiddle goes home to lobby-7 
mr-bigglesworth moves from grendels-den to lobby-10 
At lobby-10 mr-bigglesworth says -- Hi course-6-frosh grendel 
At lobby-10 mr-bigglesworth says -- What are you doing still up? Everyone back to their rooms! 
At lobby-10 course-6-frosh goes home to building-13 
At lobby-10 grendel goes home to lobby-10 
grendel moves from lobby-10 to lobby-7 
At lobby-7 grendel says -- Hi ben-bitdiddle registrar 
grendel moves from lobby-7 to student-center 
registrar moves from lobby-7 to lobby-10 
At lobby-10 registrar says -- Hi mr-bigglesworth 
registrar moves from lobby-10 to building-13 
At building-13 registrar says -- Hi course-6-frosh 
At building-13 registrar takes a bite out of course-6-frosh 
At building-13 course-6-frosh says -- Ouch! 1 hits is more than I want! 
;Value: done

1 ]=> (run-clock 1)

--- the-clock Tick 1 --- 
ben-bitdiddle moves from lobby-7 to lobby-10 
At lobby-10 ben-bitdiddle says -- Hi mr-bigglesworth 
ben-bitdiddle moves from lobby-10 to great-court 
At great-court ben-bitdiddle says -- I try but cannot take lovely-trees 
At great-court ben-bitdiddle wave the shortest-wand toward slug-spell , incant dagnabbit ekaterin 
At stata-center nothing happend. slug-spell is not a person 
alyssa-hacker moves from legal-seafood to great-court 
At great-court alyssa-hacker says -- Hi ben-bitdiddle 
At great-court alyssa-hacker says -- I try but cannot take flag-pole 
At great-court alyssa-hacker wave the shortest-wand toward ben-bitdiddle , incant dagnabbit ekaterin 
At great-court A slug comes out of ben-bitdiddle 's mouth. 
course-6-frosh moves from building-13 to lobby-10 
At lobby-10 course-6-frosh says -- Hi mr-bigglesworth 
At lobby-10 course-6-frosh wave the shortest-wand toward mr-bigglesworth , incant dagnabbit ekaterin 
At lobby-10 A slug comes out of mr-bigglesworth 's mouth. 
lambda-man moves from 6001-lab to eecs-hq 
lambda-man moves from eecs-hq to 6001-lab 
At 6001-lab lambda-man says -- I take a-ring-of-obfuscation from 6001-lab 
At 6001-lab lambda-man wave the shortest-wand toward slug-spell , incant dagnabbit ekaterin 
At stata-center nothing happend. slug-spell is not a person 
susan-hockfield moves from lobby-10 to great-court 
At great-court susan-hockfield says -- Hi alyssa-hacker ben-bitdiddle 
At great-court susan-hockfield says -- I take a-ring-of-obfuscation from great-court 
At great-court susan-hockfield wave the shortest-wand toward alyssa-hacker , but nothing happened. 
At great-court susan-hockfield teaches alyssa-hacker the spell -- boil-spell 
eric-grimson moves from baker to bexley 
eric-grimson moves from bexley to baker 
At baker eric-grimson says -- I take slug-spell from baker 
At baker eric-grimson wave the shortest-wand toward slug-spell , incant dagnabbit ekaterin 
At stata-center nothing happend. slug-spell is not a person 
dr-evil moves from 10-250 to barker-library 
At barker-library dr-evil says -- What are you doing still up? Everyone back to their rooms! 
mr-bigglesworth moves from lobby-10 to grendels-den 
At grendels-den mr-bigglesworth says -- What are you doing still up? Everyone back to their rooms! 
grendel moves from student-center to bexley 
grendel moves from bexley to baker 
At baker grendel says -- Hi eric-grimson 
grendel moves from baker to bexley 
At bexley grendel 's belly rumbles 
registrar moves from building-13 to lobby-10 
At lobby-10 registrar says -- Hi course-6-frosh 
registrar moves from lobby-10 to building-13 
;Value: done


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; computer exercise 9 tests
;; test counterspell

(load "objsys.scm")
(load "objtypes.scm")
(load "setup.scm")
(setup 'cheky)

1 ]=> (ask me 'look-around)
You are in barker-library 
You are holding: slug-counterspell wind-counterspell 
You see stuff in the room: a-ring-of-obfuscation boil-spell engineering-book 
There are no other people around you. 
The exits are in directions: down 
;Value: ok

1 ]=> (ask (thing-named 'boil-spell) 'use me me)
At barker-library cheky grows boils on their nose 
;Value: message-displayed

1 ]=> (ask (thing-named 'boil-spell) 'use me me)
At barker-library cheky grows boils on their nose 
;Value: message-displayed

1 ]=> (ask me 'take (create-counterspell 'boil-counterspell 'boil-spell
				   (ask me 'location)))
At barker-library cheky says -- I take boil-counterspell from barker-library 
;Value 48: (instance #[compound-procedure 49 handler])

;; !ck! nothing happened here, prove that my boil-counterspell works
1 ]=> (ask (thing-named 'boil-spell) 'use me me)   
;Unspecified return value 





