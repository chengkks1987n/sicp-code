(load "objsys.scm")
(load "objtypes.scm")
(load "setup.scm")

(define (instance-handler? objs)
  (cond ((null? objs) #t)
	((or (handler? (car objs)) (instance? (car objs))) 
	 (instance-handler? (cdr objs)))
	(else #f)))
      
  (if (not (instance-handler? (list obj)))
      (error "in safe-ask:" default-value obj msg args))


 (setup 'ben-bitdiddle)
(ask clock 'tick)


(ask clock 'the-time)
 (run-clock 5)
 (ask screen 'DEITY-MODE #f)
 (ask screen 'DEITY-MODE #t)
 (ask me 'look-around)
 (ask me 'take (thing-named 'engineering-book))
 (ask me 'go 'up)
 (ask me 'go 'down)
 (ask me 'go 'north)
 (ask me 'go 'east)

 (show me)
 (show screen)
 (show clock)
 (pp me)



