;;; computer exercise 5 tests
;; give the command and the outpu result after the (setup 'cheky)

(load "objsys.scm")
p(load "objtypes.scm")
(load "setup.scm")
(setup 'cheky)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test zap and wave of the wand 

1 ]=> (ask me 'look-around)

You are in stata-center 
You are not holding anything. 
You see stuff in the room: a-ring-of-obfuscation slug-spell sicp 
There are no other people around you. 
The exits are in directions: down up south north west 
;Value: ok

1 ]=> (create-wand 'black-wand (ask me 'location))

1 ]=> (ask me 'look-around)

You are in stata-center 
You are not holding anything. 
You see stuff in the room: black-wand a-ring-of-obfuscation slug-spell sicp 
There are no other people around you. 
The exits are in directions: down up south north west 
;Value: ok

1 ]=> (ask (thing-named 'black-wand) 'zap (pick-random (ask me 'people-around)))

;Unspecified return value

1 ]=> (ask (thing-named 'black-wand) 'wave)

;Unspecified return value

1 ]=> (ask me 'take (thing-named 'black-wand))

At stata-center cheky says -- I take black-wand from stata-center 
;Value 33: (instance #[compound-procedure 34 handler])

1 ]=> (ask (thing-named 'black-wand) 'zap (pick-random (ask me 'stuff-around)))

At stata-center cheky wave the black-wand toward slug-spell , but nothing happened. 
;Value: message-displayed

1 ]=> (ask (thing-named 'black-wand) 'wave)

At stata-center cheky wave the black-wand toward sicp , but nothing happened. 
;Value: message-displayed

1 ]=> (ask me 'take (thing-named 'slug-spell))

At stata-center cheky says -- I take slug-spell from stata-center 
;Value 39: (instance #[compound-procedure 37 handler])

1 ]=> (ask (thing-named 'black-wand) 'zap (pick-random (ask me 'stuff-around)))

At stata-center cheky wave the black-wand toward a-ring-of-obfuscation , incant  dagnabbit ekaterin 
At stata-center A slug comes out of a-ring-of-obfuscation 's mouth. 
;Value 40: (instance #[compound-procedure 41 handler])

1 ]=> (ask (thing-named 'black-wand) 'wave)

At stata-center cheky wave the black-wand toward a-ring-of-obfuscation , incant  dagnabbit ekaterin 
At stata-center A slug comes out of a-ring-of-obfuscation 's mouth. 
;Value 42: (instance #[compound-procedure 43 handler])

1 ]=> (ask me 'look-around)

You are in stata-center 
You are holding: slug-spell black-wand 
You see stuff in the room: slug slug a-ring-of-obfuscation sicp 
There are no other people around you. 
The exits are in directions: down up south north west 
;Value: ok
