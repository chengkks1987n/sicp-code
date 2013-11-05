
;;; MIT 6.001                               Spring, 2005
;;; PROJECT 3

(define *search-debug* #t)         ; flag that shows search progress

;;; Searching and Indexing the World Wide Web.
;;;
;;; This file contains three major components, all of which are
;;; *not* web specific.  They are general purpose abstractions
;;; that we will then use to represent, search, and index the web.
;;;
;;;  1. Graph Abstraction -- directed graph with labeled nodes,
;;;                          node children (outgoing edges), and
;;;                          node contents
;;;
;;;  2. Search and        -- system to search a graph network looking
;;;     Search Strategy      for some goal
;;;
;;;  3. Index             -- an index associating a key with
;;;                          one or more values

;;;------------------------------------------------------------
;;; Graph Abstraction
;;;
;;;   Graph                     a collection of Graph-Elements
;;;   Graph-Element               a node, outgoing children from the
;;;                               node, and contents for the node
;;;   Node = symbol             a symbol label or name for the node
;;;   Contents = anytype        the contents for the node

;;---------------
;; Graph-Element

; make-graph-element: Node,list<Node>,Contents -> Element
(define (make-graph-element node children contents)
  (list 'graph-element node children contents))

(define (graph-element? element)            ; anytype -> boolean
  (and (pair? element) (eq? 'graph-element (car element))))

; Get the node (the name) from the Graph-Element
(define (graph-element->node element)       ; Graph-Element -> Node
  (if (not (graph-element? element))
      (error "object not element: " element)
      (first (cdr element))))

; Get the children (a list of outgoing node names)
; from the Graph-Element
(define (graph-element->children element)   ; Graph-Element -> list<Node>
  (if (not (graph-element? element))
      (error "object not element: " element)
      (second (cdr element))))

; Get the contents from the Graph-Element
(define (graph-element->contents element)   ; Graph-Element -> Contents
  (if (not (graph-element? element))
      (error "object not element: " element)
      (third (cdr element))))

;;---------------
;; Graph

(define (make-graph elements)            ; list<Element> -> Graph
  (cons 'graph elements))

(define (graph? graph)                  ; anytype -> boolean
  (and (pair? graph) (eq? 'graph (car graph))))

(define (graph-elements graph)           ; Graph -> list<Graph-Element>
  (if (not (graph? graph))
      (error "object not a graph: " graph)
      (cdr graph)))

(define (graph-root graph)		; Graph -> Node|null
  (let ((elements (graph-elements graph)))
    (if (null? elements)
	#f
	(graph-element->node (car elements)))))

; Find the specified node in the graph
(define (find-graph-element graph node)   ; Graph,Node -> Graph-Element|null
  (define (find elements)
    (cond ((null? elements) '())
          ((eq? (graph-element->node (car elements)) node)
           (car elements))
          (else (find (cdr elements)))))
  (find (graph-elements graph)))

; Find the children of the specified node in the graph
(define (find-node-children graph node)        ; Graph,Node -> list<Node>|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->children element)
        '())))

; Find the contents of the specified node in the graph
(define (find-node-contents graph node)         ; Graph,Node -> Contents|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->contents element)
        '())))

;; Testing...

(define test-graph
  (make-graph (list
   (make-graph-element 'a '(b i m) '(some words))
   (make-graph-element 'b '(c d e h) '(more words))
   (make-graph-element 'c '() '(at c node some words))
   (make-graph-element 'd '() '())
   (make-graph-element 'e '(f g) '(and even more words))
   (make-graph-element 'f '() '())
   (make-graph-element 'g '() '())
   (make-graph-element 'h '() '())
   (make-graph-element 'i '(j k l) '(more words yet))
   (make-graph-element 'j '() '())
   (make-graph-element 'k '() '())
   (make-graph-element 'l '() '()))))

(define test-cycle
  (make-graph (list
   (make-graph-element 'a '(b c) '(words for node a))
   (make-graph-element 'b '(c) '(words for node b))
   (make-graph-element 'c '(a) '(words for node c)))))

 (find-graph-element test-graph 'b)
 (find-graph-element test-graph 'z)
 (find-node-children test-graph 'b)
 (find-node-children test-graph 'z)
 (find-node-contents test-graph 'b)
 (find-node-contents test-graph 'z)


;;;------------------------------------------------------------
;;; Searching a network
;;;
;;; We define below a standard search procedure that walks
;;; over a graph in an effort to find a desired node.
;;; This version does not handle cycles in the graph

;; search: Node, (Node->Boolean), (Graph, Node -> List<Node>)
;;         (List<Node>, List<Node> -> List<Node>), Graph
;;           --> Boolean 

(define (search initial-state goal? successors merge graph)
  ;; initial-state is the start state of the search
  ;;
  ;; goal? is the predicate that determines whether we have
  ;; reached the goal
  ;;
  ;; successors computes from the current state all successor states
  ;;
  ;; merge combines new states with the set of states still to explore
  (define (search-inner still-to-do)
    (if (null? still-to-do)
	#f
	(let ((current (car still-to-do)))
	  (if *search-debug*
	      (write-line (list 'now-at current)))
	  (if (goal? current)
	      #t
	      (search-inner
	       (merge (successors graph current) (cdr still-to-do)))))))
  (search-inner (list initial-state)))

(define (DFS-simple start goal? graph)
  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append new old))
	  graph))


 (DFS-simple 'a
             (lambda (node) (eq? node 'z))
             test-graph)

;;; infinite loop  
;;(DFS-simple 'a
;;	    (lambda (node) (eq? node 'd))
;;	    test-cycle)


;; you will need to write a similar search procedure that handles cycles
(define (dfs start goal? graph)
  (let ((visited '()))
    (define (get-unvisited nodes)
      (if (null? nodes)
	  '()
	  (if (memq (car nodes) visited)
	      (get-unvisited (cdr nodes))
	      (cons (car nodes) (get-unvisited (cdr nodes))))))
    (define (succe g cur)
      (set! visited (cons cur visited))
      (let ((nodes (find-node-children g cur)))
	(get-unvisited nodes)))
    (search start goal? succe
	    (lambda (new old) (append new old))
	    graph)))

(dfs 'a (lambda (n) (eq? n 'z)) test-graph)
(dfs 'a (lambda (n) (eq? n 'd)) test-cycle)

;; my bfs
(define (BFS-simple start goal? graph)
  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append old new))
	  graph))

(define (bfs start goal? graph)
  (let ((visited '()))
    (define (get-unvisited nodes)
      (if (null? nodes)
	  '()
	  (if (memq (car nodes) visited)
	      (get-unvisited (cdr nodes))
	      (cons (car nodes) (get-unvisited (cdr nodes))))))
    (define (succe g cur)
      (set! visited (cons cur visited))
      (let ((nodes (find-node-children g cur)))
	(get-unvisited nodes)))
    (search start goal? succe
	    (lambda (new old) (append old new))
	    graph)))

(bfs 'a (lambda (n) (eq? n 'z)) test-graph)
(bfs 'a (lambda (n) (eq? n 'a)) test-cycle)    
(bfs 'a (lambda (n) (eq? n 'c)) test-cycle)    
(bfs 'a (lambda (n) (eq? n 'd)) test-cycle)    

(define (search-with-cycles initial-state goal? successors merge graph)
  ;; initial-state is the start state of the search
  ;;
  ;; goal? is the predicate that determines whether we have
  ;; reached the goal
  ;;
  ;; successors computes from the current state all successor states
  ;;
  ;; merge combines new states with the set of states still to explore
  (let ((visited '()))
    (define (get-unvisited nodes)
      (if (null? nodes)
	  '()
	  (if (memq (car nodes) visited)
	      (get-unvisited (cdr nodes))
	      (cons (car nodes) (get-unvisited (cdr nodes))))))
    (define (search-inner still-to-do)
      (if (null? still-to-do)
	  #f
	  (let ((current (car still-to-do)))
	    (if *search-debug*
		(write-line (list 'now-at current)))
	    (if (memq current visited)
		(search-inner (cdr still-to-do))
		(begin 
		  (set! visited (cons current visited))
		  (if (goal? current)
		      #t
		      (search-inner
		       (merge 
			(get-unvisited (successors graph current))
			(cdr still-to-do)))))))))
    (search-inner (list initial-state))))

(define (new-dfs start goal? graph)
  (search-with-cycles start goal? find-node-children 
		      (lambda (new old) (append new old))
		      graph))

(define (new-bfs start goal? graph)
  (search-with-cycles start goal? find-node-children 
		      (lambda (new old) (append old new))
		      graph))
(new-bfs 'a (lambda (n) (eq? n 'z)) test-graph)
(new-bfs 'a (lambda (n) (eq? n 'a)) test-cycle)    
(new-bfs 'a (lambda (n) (eq? n 'c)) test-cycle)    
(new-bfs 'a (lambda (n) (eq? n 'd)) test-cycle)    

(new-dfs 'a (lambda (n) (eq? n 'z)) test-graph)
(new-dfs 'a (lambda (n) (eq? n 'a)) test-cycle)    
(new-dfs 'a (lambda (n) (eq? n 'c)) test-cycle)    
(new-dfs 'a (lambda (n) (eq? n 'd)) test-cycle)    

;;;------------------------------------------------------------
;;; Index Abstraction
;;;
;;;   An Index enables us to associate values with keys, and
;;; to retrieve those values later on given the key.
;;;
;;; Key = symbol
;;; Val = symbol

;; Index Implementation
;;
;;   An Index will be a tagged data object that holds a 
;; list of Index-Entries.  Each Index-Entry associates
;; a key with a list of values for that key, i.e.
;;   Index = Pair<Index-Tag, list<Index-Entry>>
;;   Index-Entry = list<Key, list<Val>>
;; 

(define (make-index)            ; void -> Index
  (list 'index))

(define (index? index)          ; antype -> boolean
  (and (pair? index) (eq? 'index (car index))))

; An index can be reset to empty.
(define (reset-index! index)    ; Index -> Index
  (cond ((not (index? index))
         (error "object not an index: " index))
        (else (set-cdr! index '())
              index)))
      
; This is an internal helper procedure not to
; be used externally.
(define (find-entry-in-index index key)
  (if (not (index? index))
      (error "object not an index: " index)
      (let ((entry (assv key (cdr index))))
        (if entry entry '()))))


; returns a list of values associated with key
(define (find-in-index index key)       ; Index,Key -> list<Val>
  (let ((index-entry (find-entry-in-index index key)))
    (if (not (null? index-entry))
        (cadr index-entry)
        '())))

;; TO BE IMPLEMENTED
(define (add-to-index! index key value) ; Index,Key,Val -> Index
  (let ((index-entry (find-entry-in-index index key)))
    (if (null? index-entry)
      ;; no entry -- create and insert a new one...
	;... TO BE IMPLEMENTED
	(let ((new-entry (list key (list value))))
	  (set-cdr! index (cons new-entry (cdr index))))
      ;; entry exists -- insert value if not already there...
	;... TO BE IMPLEMENTED
	(set-car! (cdr index-entry) (cons value (cadr index-entry)))))
  index)

;; Testing
 (define test-index (make-index))
 (add-to-index! test-index 'key1 'value1)
 (add-to-index! test-index 'key2 'value2)
 (add-to-index! test-index 'key1 'another-value1)
 
 (find-in-index test-index 'key1)
 (find-in-index test-index 'key2)
 (find-in-index test-index 'key)


;;------------------------------------------------------------
;; Finally, the Web!

;;--------------------
;; Web representation 
;;
;; We'll represent a "Web" as a graph.  Each Node in
;; the graph will be a URL; the node contents is the
;; Text inside the URL, and the node children is the
;; list of URL links inside the URL:
;;
;; Web = Graph
;; URL = Node
;; Text = list<Word>
;; Word = symbol      

; Procedures to get web links and web page contents:

(define (find-URL-links web url)
  (find-node-children web url))

(define (find-URL-text web url)
  (find-node-contents web url))


;; The real definition of THE-WEB we'll use is in another file, 
;; including all of the words in the documents.

(define (2sym x)
  (cond ((symbol? x) x)
	((string? x) (string->symbol x))
	((number? x) (string->symbol (number->string x)))
	(else (error x))))

(define the-web
  (make-graph (list
    (make-graph-element 
     'http://sicp.csail.mit.edu/
     '(http://sicp.csail.mit.edu/SchemeImplementations
       http://sicp.csail.mit.edu/psets)
     (map 2sym '(18:30:02 2004 6001-WEBMASTER@CSAIL.MIT.EDU 8 
			  ABOUT ALL AM AND ANNOUNCEMENTS ANSWERS ARE ASSIGNMENT 
			  ASSIGNMENTS BY CALENDAR CAN CHANGE COLLABORATIVE 
			  COMMENTS COMPUTER COPYRIGHT CURRENT DO DOCUMENTATION 
			  EDT FALL FIND FOR GENERAL GET GETTING GUIDELINES HELP
			  HOW I IN INDIVIDUAL INFORMATION INSTITUTE INTERPRETATION 
			  IS LAST LECTURE MASSACHUSETTS ME MICROQUIZZES MODIFIED 
			  MY NEW NOTES OCT OF ON ON-LINE ORAL OWN PAST POLICY 
			  POSTED PRESENTATIONS PREVIOUS PROBLEM PROGRAMS 
			  RECITATION RECITATIONS RECORDS RESERVED RIGHTS SCHEME 
			  SECTION SECTIONS SEND SET SETS SITE SOFTWARE STAFF
			  STRUCTURE SUBJECT TECHNOLOGY TELL TERMS THE THIS THU TO
			  UP USE WEEK WHAT WHERE WHICH WORK WRITING)))
    (make-graph-element
     'http://sicp.csail.mit.edu/SchemeImplementations
     '(http://sicp.csail.mit.edu/getting-help.html
       http://sicp.csail.mit.edu/lab-use.html
       *the-goal*)
     (map 2sym '(11:09 2004 3.1 34-501 4.0 6.001 6001-WEBMASTER@CSAIL.MIT.EDU 
            7 7.5A 95 98 A ABOUT ACCESS ADDITION ALL ALSO AN AND ANY 
            ARE ASSIGNMENTS ASSISTANTS AT ATHENA AVAILABLE BASED BE 
            BEAR BEAUTY BECAUSE BEEN BEFORE BETWEEN BUT BY CAN 
            CAPABLE CERTAIN COME COMFORT COMMENTS CONVENIENT COPY 
            COPYRIGHT COURSE CROWDED DEBUGGER DISK DISTRIBUTIONS
            DO DOCUMENTATION DONE DRSCHEME DUE EDITOR EDSCHEME EITHER
            ENJOY ETC EXTENSIONS FEBRUARY FEE FEEL FELLOW FILES FIND 
            FLOPPY FOLLOWING FOR FREE FROM GET GNU GRANT HAS HAVE HELP
            HERE HOME IDENTICAL IF IMPLEMENTATIONS IN INC INCLUDED 
            INCLUDING INSTALL INSTITUTE IS IT JUST LAB LAST LIKE LINUX 
            LOCKER LOT MAC MACHINE MASSACHUSETTS MAY MIND MODIFIED MUST 
            NEED NEWER NOT NOTE NT OBTAINED OF OFTEN OLDER ON OPTIONS 
            OR OTHER OUT OWN PAGE PARTIALLY PC PERHAPS PLATFORMS PLEASE 
            PM PREPARED PREVIOUS PREVIOUSLY PROBLEM PROGRESS REALIZE 
            REPRODUCE REQUIRES RESERVED RETURN RICE RIGHTS ROOM ROOT 
            RUN RUNNING SAVE SCENIC SCHEME SCHEMERS SEE SEMESTERS SEND 
            SETS SEVERAL SHOULD SINCE SITE SMALL SOMEONE SPRING STAFF 
            STUDENTS SUGGEST SUPPORTED SYSTEM SYSTEMS TECHNOLOGY TESTED 
            THAN THAT THE THEM THERE THESE THIS TIME TO TOOK TRANSFERRING
            TRY UNIVERSITY UNIX UNSUPPORTED UPDATE USE USED USING VERSION
            VERSIONS VERSON VERY VIRTUALLY WAIVERS WANT WARNED WE WEB 
            WHERE WHO WILL WINDOWS WITH WORK WORKSTATIONS WOULD YOU YOUR)))
    (make-graph-element
     'http://sicp.csail.mit.edu/psets
     '()
     (map 2sym '(0 1 15 2004 2 20 23:32:29 28 3 4 5 6.001
        6001-WEBMASTER@CSAIL.MIT.EDU 98 A ABOUT ALL ALSO AND ARE AS 
        ASSIGNMENTS ATHENA AVAILABLE BETWEEN BOTH BY CAN COLLABORATIVE 
        COMMENTS COPYRIGHT DISTRIBUTED EDT FALL FILES FOR FORMAT GET 
        GHOSTVIEW HELP HERE HOME HOW HTML I IN INSTITUTE IS LAB LAST 
        LECTURE LOCKER MASSACHUSETTS MODIFIED MY OCT OF ON PAGE POLICY 
        POSTED POSTSCRIPT PRINTING PROBLEM REQUIRES RESERVED RETURN 
        RIGHTS SCREEN SEND SEPT SET SETS SHOULD SITE SOLUTIONS SUCH 
        TECHNOLOGY THE THEY THIS THU TO TRANSFER TUESDAYS UP VIEWER
        VIEWING WEB WHAT WHERE WHICH WITH WORK WRITE)))
    (make-graph-element
     'http://sicp.csail.mit.edu/getting-help.html
     '(http://sicp.csail.mit.edu/
       http://sicp.csail.mit.edu/SchemeImplementations)
     (map 2sym '(09:38:18 1 10 2004 2 23 24 339-0052 4 5 6 6.001 6001-HELP@MIT.EDU
               6001-WEBMASTER@CSAIL.MIT.EDU 8 947-2394 98 A ABLE ABOUT 
               ADJUSTING ADMINISTRATIVE ADVANTAGE ALL ALSO AM AN AND 
               ANY ARE AS ASSISTANTS AT ATTENTION BE BEEPER BEING BROUGHT
               BY CAN CANNOT CELL COMMENTS COMPUTER COPYRIGHT COURSE 
               CURRENTLY DAY DEMAND DISCUSSION DOES DONE DURING DUTY
               EARLY EDT EDUCATION EMAIL FALL FEEL FOLLOWS FOR FORUM 
               FRIDAY GET GETTING GRIPE HAS HAVE HELP HERE HOME HOMEWORK
               HOURS HOW IF IN INFORMATION INSTALL INSTITUTE INSTRUCTOR 
               IS IT LAB LAST LECTURERS LINE LOST MASSACHUSETTS MIDNIGHT
               MIDNITE MIGHT MINORITY MODIFIED MONDAY NATURE NEED NIGHT
               NOT OF OFFICE ON OPEN OPERATES OR OTHER PAGE PERSONAL 
               PHONE PHONING PLEASE PM PROBLEM PROBLEMS PROGRAM REACH
               RECITATION REQUEST RESERVED RESPONSE REST RETURN RIGHTS 
               SATURDAY SCHEME SCREAMS SECRETARY SEE SEMESTER SEND SEP
               SET SETUP SHOULD SITE SOME SPECIFIC STAFFED STAFFING 
               START STATEMENT STUDENT SUNDAY TECHNOLOGY THAT THE THESE
               THINGS THIS THURSDAY TO TOUCH TRY TUESDAY TUTOR TUTORING
               UNDERSTAND UNTIL WAIT WAY WE WED WEDNESDAY WHICH WILL
               WITH YOU YOUR)))
    (make-graph-element
     'http://sicp.csail.mit.edu/lab-use.html
     '()
     (map 2sym '(1 2004 2 24 34-501 4:33 6.001 6001-WEBMASTER@CSAIL.MIT.EDU 7 8 A
        ABLE ABOUT ACCESSIBLE ADDITIONAL AFTER ALL ALLOWING ALSO AM AN 
        AND ANY APPRECIATED ARE ARRANGED AS ASSISTANT AT BE BETWEEN 
        BOSTON BROUGHT BY CAB CAMPUS CAREFUL CAUTION CLASSES CLEAN CODE
        COMMENTS COMMON COMPONENTS CONSIDER CONSIDERATION CONTACT 
        COOPERATION COPYRIGHT CORRIDOR DAY DAYS DETAILS DO DOING
        DOORS DRINK DURING EDWIN ESCORT ESPECIALLY EXERCISE EXPERIMENT 
        EXTEND FEBRUARY FOLLOWING FOOD FOR FRATS FUTURE GETTING GO 
        GREATLY HAVE HERE HOME HOURS HOWEVER IF IN INFORMATION INNER 
        INSTITUTE INSTRUMENT INTO IS ISSUES KEYBOARDS LAB LABORATORY 
        LAST LATE LEADING LIVE LOOK MACHINES MANUAL MASSACHUSETTS
        MIDNIGHT MODIFIED MUST NEAR NEED NIGHT NOT NOTE OF OFF ON ONE 
        OPEN OPERATION OTHER OUT OUTER PAGE PANIC PAST PATROL PERSONAL
        PLEASE PM POLICY PROBLEM PROVIDED REMEMBER RESERVED
        RESPONSIBILITY RETURN RETURNING RIDE RIGHTS RIVER ROOM SAFE
        SAFETY SCHEME SECOND SEE SEND SENSE SET SHOW SIDE SITE SMOOTHLY
        SO SPELLED STAFFED STARTED SUCH TAKING TECHNOLOGY TERM THAT 
        THE THEM THINGS THIS TIME TO UP USE USING WE WEEK WILL WITH 
        WORKING YOU YOUR YOURSELF ))))))

;;(define the-web
;;  (list
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/
;;    '(http://sicp.csail.mit.edu/SchemeImplementations/
;;      http://sicp.csail.mit.edu/projects/)
;;    '(... words extracted from http://sicp.csail.mit.edu/ ...))
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/projects/
;;    '(http://sicp.csail.mit.edu/collaborative-work.html
;;      http://sicp.csail.mit.edu/getting-help.html)
;;    '(... words extracted from http://sicp.csail.mit.edu/SchemeImplementations/ ...))
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/getting-help.html
;;    '(http://sicp.csail.mit.edu/
;;      http://sicp.csail.mit.edu/SchemeImplementations/)
;;    '(... words extracted from http://sicp.csail.mit.edu/getting-help.html))
;;   ))
;;

;;--------------------
;; Searching the Web

;; you need to write expressions to search the web using different search
;; strategies


;;--------------------
;; Indexing the Web
;;
;;   Our purpose in creating an index of a web is to
;; later support the ability to find any pages that contain
;; a given word.  Thus, a Key in our index will be a Word,
;; and the values in the index will be the URLs of pages
;; that contain that word.

;; A procedure to help  with indexing web pages
;; using the Index abstraction.  The idea is to
;; get the text associated with the URL from the
;; web, and then key each of the words in the
;; text into the index.

;; TO BE IMPLEMENTED
;; add-document-to-index!: Index, Web, URL
(define (add-text-to-index index text url)
  (if (not (null? text))
      (begin 
	(add-to-index! index (car text) url)
	(add-text-to-index index (cdr text) url))))

 (define (add-document-to-index! index web url)
   (let ((text (find-url-text web url)))
     (add-text-to-index index text url)))



;; Example use
;; 
 (define the-web-index (make-index))
 
 (add-document-to-index! the-web-index
                         the-web 
                         'http://sicp.csail.mit.edu/)
 
 (find-in-index the-web-index 'help)
 ;Value: (http://sicp.csail.mit.edu/)
 
 (find-in-index the-web-index '*magic*)
 ;Value: #f

;;;-----------------------------
;;; computer exercise 5
;;; give a web and a initial url, add the documents of this initial url to a index,
;;; and add other related urls' documents to the index too.
;;; return a procedure, its type is (Word -> URL)
;;; web = graph
;;; init-url = Node
;;; word = symbol
;;; documents = List<Word>

(define (make-web-index web init-url)
  (let ((index (make-index)))
    (define (goal? url)
      (add-document-to-index! index web url)
      #f)
    (bfs init-url goal? web) ; add all documents to index, start from init-url
    (lambda (word)
      (find-in-index index word))))

(define find-documents (make-web-index the-web 'http://sicp.csail.mit.edu/))
(find-documents 'help)
(find-documents 'collaborative)

;;; computer exercise 6
(define (search-any web start-node word)
  (let ((ans '()))
    (define (goal? node)
      (let ((node-index (make-index)))
	(add-document-to-index! node-index web node)
	(if (not (null? (find-in-index node-index word)))
	    (begin (set! ans node)
		   #t)
	    #f)))
    (bfs start-node goal? web)
    ans))

(search-any the-web 'http://sicp.csail.mit.edu/ 'help)
(search-any the-web 'http://sicp.csail.mit.edu/lab-use.html 'help)
(search-any the-web 'http://sicp.csail.mit.edu/ 'collaborative)
(search-any the-web 'http://sicp.csail.mit.edu/psets 'collaborative)

(define (search-all web start-node word)
  (let ((ans '()))
    (define (goal? node)
      (define node-index (make-index))
      (add-document-to-index! node-index web node)
      (if (not (null? (find-in-index node-index word)))
	  (begin (set! ans (cons node ans))
;		 (display (find-in-index node-index word))
;		 (newline)
		 #f)
	  #f))

    (bfs start-node goal? web)
    ans))

(search-all the-web 'http://sicp.csail.mit.edu/ 'help)
(search-all the-web 'http://sicp.csail.mit.edu/ 'collaborative)

;;------------------------------------------------------------
;; utility for timing procedure calls.
;; returns the time in seconds

(define (timed f . args)
  (let ((start (runtime)))
    (let ((val (apply f args)))
      (newline)
      (display "time expended: ")
      (display (- (runtime) start))
      val)))

;;; computer exercise 7
(load "generate.scm")

(define random-web (generate-random-web 200))

(timed search-any random-web '*start* 'help)
;=> time expended: 9.999999999999787e-3
(timed search-any random-web '*start* 'Susanhockfield)
;=> time expended: 1.3600000000000003

(timed search-all random-web '*start* 'help)
;=> time expended: 1.4100000000000001
(timed search-any random-web '*start* 'Susanhockfield)
;=> time expended: 1.3800000000000026

(define random-web-index (make-web-index random-web '*start*))
(timed make-web-index random-web '*start*)
;=> time expended: 1.7400000000000002
(timed random-web-index 'help)
;=> time expended: 0.
(timed random-web-index 'Susanhockfield)
;=> time expended: 0.


;;; conclusion
; index-search spend lots of time on make the whole index,
; but it spends only a little time on search.
; if the web is static(dont change very much), its a better choice.
;
; the dynamic-seatch spends shorter time when you search only one target
; and the target is in  front of the web. if the target is at the last of 
; the web or it is not in the web, all the dynamic-search spend the same time. 
;

;;; computer exercise 8
(define (optimized-index ind)
  (define (compare-key e1 e2)
    (symbol<? (car e1) (car e2)))
    
  (if (not (index? ind))
      (error "the arg is not an index: " ind)
      (let ((v (list->vector (cdr ind))))
	(sort! v compare-key)
	(cons 'optimized-index v))))

(define (optimized-index? optind)
  (and (pair? optind) (eq? (car optind) 'optimized-index)))

(define opt-ind (optimized-index test-index))

;;; problem 9
(define (find-entry-in-optimized-index optind key)
  (if (not (optimized-index? optind))
      (error "the first arg is not optimized-index: " optind key)
      (vector-binary-search (cdr optind) symbol<? car key)))

(find-entry-in-optimized-index opt-ind 'key1)
(find-entry-in-optimized-index opt-ind 'key2)
(find-entry-in-optimized-index opt-ind 'key)

(define (find-in-optimized-index optind key)
  (let ((e (find-entry-in-optimized-index optind key)))
    (if e
	(cadr e)
	'())))

(find-in-optimized-index opt-ind 'key)
(find-in-optimized-index opt-ind 'key1)
(find-in-optimized-index opt-ind 'key2)

;;
(define (make-optimized-web-index web init-url)
  (let ((index (make-index)))
    (define (goal? url)
      (add-document-to-index! index web url)
      #f)
    (bfs init-url goal? web) ; add all documents to index, start from init-url
    (let ((opt-ind (optimized-index index)))
      (lambda (word)
	(find-in-optimized-index opt-ind word)))))

(define find-optimized-documents (make-optimized-web-index random-web '*start*))
(timed make-optimized-web-index random-web '*start*)
(timed find-optimized-documents 'help) 
;=> time expended: 0.
(timed find-optimized-documents 'Sunsanhockfield) 
;=> time expended: 0.


;; repeat running procedure (f args) for count times
(define (repeat count f . args)
  (if (> count 0)
      (begin (apply f args)
	     (apply repeat (- count 1) f args))
      (apply f args)))

(timed repeat 999 find-optimized-documents 'Sunsanhockfield)
;=> time expended: .01999999999999602
(timed repeat 999 random-web-index 'Sunsanhockfield)
;=> time expended: .03999999999999915

  

