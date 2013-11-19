;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name:Alvin Wang, Brent Yoshida, Taylor Kennedy   Date: 10/23/13
;;; Course: ICS313        Assignment: 6
;;; File: CodeMonkey.lisp

;;;;;;;;;;;;;;;;;;
; Global Objects ;
;;;;;;;;;;;;;;;;;;

;;global constant containing name
(defparameter +ID+ "Alvin Wang, Brent Yoshida, and Taylor Kennedy")

;;global constants to check what floors you are currently on
(defparameter *First-Floor* t)
(defparameter *Second-Floor* nil)
(defparameter *Third-Floor* nil)
(defparameter *Street-Level* nil)
(defparameter *Jungle* nil)


;;global constants for second-floor door puzzle
(defparameter *attempts2* 1)
(defparameter *talked2* nil)

;;global constants for third-floor door puzzle
(defparameter *third-floor-solved* NIL)
(defparameter *e-door-code* 9999)
(defparameter *current-guess* 0000)
(defparameter *attempts* 1)

 
;global constants for street-level run-game
(defparameter *run-turns* 0)
(defparameter *faults* 0)
(defparameter *start-time* NIL)
(defparameter *end-time* NIL)

;;variables for description of in-game locations
(defparameter *nodes* '((first-floor (you are in an office.
                            a gorilla is sitting at the office desk.
                            The door to the next floor is locked.))
                        (second-floor (you are on the second floor. you see a long hallway and a emergency exit 
                                           door at the end an old monkey is standing in front of you and a fellow monkey is standing in front of you. 
                                           you can "talk" to him. there is a book to your left that reads "african_or_european" which swallow is faster.))
                        (break-room (you are in the break room.
                            there is a blender and freezer in the corner.))
                        (storage (you are in the storage room.
                            there is a mountain of bananas in the middle of the room.))
                        (third-floor (you are on the third floor. there are no rooms of interest. you see a locked emergency exit door. perhaps you can decipher it.))
                        (street-level (you make your way down the emergency stairs. you made it out of the office building. better make a run for it.))
                        (jungle (you have made it back home to the jungle! Game over. type -quit- to exit.))
                        ))

;;variables to show connections between locations
(defparameter *edges* '((first-floor (break-room right hallway))
                        (break-room (first-floor left hallway)
                                    (storage right door))
                        (storage (break-room left door))
                        ))

;;list of visible objects
(defparameter *objects* '(bananas))

;;list of visible objects and where they are located
(defparameter *object-locations* '((bananas storage)))

;;variable used to track plater's current position, default location at living room
(defparameter *location* 'first-floor)

;;variable of allowed commands
(defparameter *allowed-commands* '(look walk pickup inventory decipher run talk))

;;;;;;;;;;;;;;;;;;;;;;
; End Global Objects ;
;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;
; Helper Functions ;
;;;;;;;;;;;;;;;;;;;;

;;describes location
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

;;desribes connecting edges
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;;used to describe multiple edges
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;list objects visible from a given location
(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

;;describe visible objects at a given location
(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;;checks if you have an object in your intentory
(defun have (object) 
    (member object (cdr (inventory))))

;;;;;;;;;;;;;;;;;;;;;;;;
; End Helper Functions ;
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;
; Functions ;
;;;;;;;;;;;;;

;;look function calls all description functions: location, paths, and objects
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;;the walk function takes a direction and lets us walk there
(defun walk (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))

;;lets us pick up objects in the world.
(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t '(you cannot get that.))))

;;shows what objects we are carrying
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;;;;;;;;;;;;;;;;;
; End Functions ;
;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;
; Game Controllers ;
;;;;;;;;;;;;;;;;;;;;

;;used to start game and initialize a custom REPL
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
          (cond
            ((or (eq (car cmd) 'help) (eq (car cmd) 'h) (eq (car cmd) '?))
             (help-menu)
             (game-repl))
            (t
            (game-print (game-eval cmd))
            (game-repl))))))

;;displays help-menu
(defun help-menu ()
  (princ "Help and Hints!")
  (terpri)
  (terpri)
  (princ "General commands:")
  (terpri)
  (princ "look - Gives a description of your surroundings.")
  (terpri)
  (princ "walk <direction> - Moves your character to new area of given direction")
  (terpri)
  (princ "pickup <object> - Picks up an object in the area and puts it in your inventory.")
  (terpri)
  (princ "inventory - Shows what is currently objects you are currently carrying")
  (terpri)
  (princ "help/h/? - Opens the help and hints menu.")
  (terpri)
  (princ "quit - Quit game.")
  (terpri)
  (terpri)
  (cond 
    ((eq *Jungle* t)
     (princ "Special Commands: ")
     (terpri)
     (princ "quit - Game Over. Nothing left to do.")
     (terpri))
    ((eq *Street-Level* t)
     (princ "Special Commands: ")
     (terpri)
     (princ "run - runs to nearest cargo plane.")
     (terpri))
    ((eq *Third-Floor* t)
     (princ "Special Commands: ")
     (terpri)
     (princ "decipher - decipher code to unlock emergency door.")
     (terpri))
    ((eq *Second-Floor* t)
     (princ "Special Commands: ")
     (terpri)
     (princ "talk - talk to the old monkey.")
     (terpri))
    ((eq *First-Floor* t)
     (princ "First Floor Special commands:")
     (terpri)
     (princ "blend bananas blender - blends bananas into a smoothie .")
     (terpri)
     (princ "give smoothie gorilla - give a smoothie to the gorilla.")
     (terpri)
     (princ "unlock key door - unlock the door to the next level.")
     (terpri))))

;;custom read function that concatenates parenthese around commands and a quote infront of parameters
(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;;game-eval allows only certain commands to go through
(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

;;tweak-text looks at each character in the list and modifies it as need.
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;;game-print converts symbol-based writing into properly capitalized text.
(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))

;;;;;;;;;;;;;;;;;;;;;;;;
; End Game Controllers ;
;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;
; Macros ;
;;;;;;;;;;

;;macro that creates a new game action
(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     ;(have ',subj)
                     )
                ,@body
            '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))

;;Macro the creates a new location
(defmacro new-location (location location-description) ; adds new location.
    `(if (and (listp ',location-description)
              (not (assoc ',location *nodes*))); checks to see if is a list and location does not already exist
       (pushnew '(,location ,location-description) *nodes*))) ; adds location to nodes.

;;Macro that creates a new object
(defmacro new-object (obj location)
;    `(if (assoc ',location *nodes*); check if location exists.
       `(progn
         (pushnew ',obj *objects*) ; adds object to list of objects.
         (pushnew '(,obj ,location) *object-locations*)));); adds object to list of object locations.

;;Macro that creates a new path
(defmacro new-path (location1 direction path-type location2 &optional direction2 path-type2)
  `(if
     (and 
       (assoc ',location1 *nodes*) 
       (assoc ',location2 *nodes*))
     (progn
       (cond
	     ;;Both locations already have edges
         ((and ',direction2 
               ',path-type2
               (assoc ',location1 *edges*)
               (assoc ',location2 *edges*))
            (nconc (assoc ',location1 *edges*) '((,location2 ,direction ,path-type)))
            (nconc (assoc ',location2 *edges*) '((,location1 ,direction2 ,path-type2))))
         ;;if Only location1 has edges
		 ((and ',direction2 
               ',path-type2
               (assoc ',location1 *edges*)
               (not (assoc ',location2 *edges*)))
            (nconc (assoc ',location1 *edges*) '((,location2 ,direction ,path-type)))
            (pushnew '(,location2 (,location1 ,direction2 ,path-type2)) *edges*))
	     ;;if only location2 has edges
         ((and ',direction2 
               ',path-type2
               (not (assoc ',location1 *edges*))
               (assoc ',location2 *edges*))
          (nconc (assoc ',location2 *edges*) '((,location1 ,direction2 ,path-type2)))
          (pushnew '(,location1 (,location2 ,direction ,path-type)) *edges*))
         ;;if both locations dont have edges
		 ((and ',direction2 
               ',path-type2
               (not (assoc ',location1 *edges*))
               (not (assoc ',location2 *edges*)))
          (pushnew '(,location1 (,location2 ,direction ,path-type)) *edges*)
          (pushnew '(,location2 (,location1 ,direction2 ,path-type2)) *edges*))
         ;;if only 1 edge is created and that location has no edges
		 ((not (assoc ',location1 *edges*))
            (pushnew '(,location1 (,location2 ,direction ,path-type)) *edges*))
          ;;otherwise add edge to location1
		  (t
           (nconc (assoc ',location1 *edges*) '((,location2 ,direction ,path-type))))))))

;;;;;;;;;;;;;;
; End Macros ;
;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;
; Taylors Functions ;
;;;;;;;;;;;;;;;;;;;;;

;; true if input the right amount of bananas into blend action
(defparameter *good-smoothie* NIL)

;;blends bananas into a smoothie
(game-action blend bananas blender break-room 
             (if (and (have 'bananas))
               (progn 
                 (princ "The instructions on the wall read:")
                 (terpri)
                 (princ "Use 1C slices of banana for the perfect smoothie")
                 (terpri)
                 (princ "Enter how many slices of bananas to use (in decimal): ")
                 (let ((input (read)))
                   (if (eq input 28)
                     (setq *good-smoothie* t)))
                 (new-object smoothie body)
                 '(You made a smoothie!))
               '(you do not have any bananas.)))

;;gives the smoothie to the gorilla
(game-action give smoothie gorilla first-floor
             (cond                    
               ((and (not (have 'smoothie)) (have 'bananas)) 
                    '(the gorilla likes banana smoothies not bananas 
                          he smashes you through the ceiling.))
               ((not (have 'smoothie)) '(I could really use a smoothie...))
               ((and (have 'smoothie) (not *good-smoothie*))
                '(The gorilla throws the smoothie in your face and howls
                      angrily that you should learn to read instructions.))
               (t (progn
                    (new-object key body)
                    '(You recieved the key!)))))

;;unlocks the door to the next level
(game-action unlock key door first-floor
             (if (not (have 'key))
               '(The door requires a key. The gorilla next to you seems like he knows something.
                     He seems to be muttering something about a banana smoothie.)
               (progn 
                 (new-path first-floor down elevator second-floor)
                 (walk 'down)
                 (look)
                 (setf *Second-Floor* 't)
                 (setf *location* 'second-floor)
                 (look)
                 '(The door is unlocked. You make your way down to the next floor)
                 )))
		   
;;;;;;;;;;;;;;;;;;;;;;;;;
; End Taylors Functions ;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
; Brents Functions ;
;;;;;;;;;;;;;;;;;;;;

; Brent Add your unique functions here
; Globals go in the global section at top
;; talk function checks to see if on second floor. if not does not work.
(defun talk ()
  (cond
    ((equal *location* 'second-floor)(if (equal *talked2* nil)(princ "The old monkey speaks to the other monkey To pass this way you must answer my questions 3 What is your name? The monkey answers Sir Monkihad. What is your quest. What is your favorite color? Blue no Gre... AHHHHH as the monkey falls through the floor.")
)
     (fresh-line)
     (princ "What is the (cdr (cdr (cdr list))) represented as in a single form 6 tries for all is all you have. ")
(let ((cmd (game-read)))
    (cond ((eq (car cmd) 'cdddddr)(question2))
          ((equal 6 *attempts2*) (game-over));; checks to see if exceeded guess limit.
          (t (setq *attempts2* (+ *attempts2* 1))(setf *talked2* 't)(talk));; if not over guess limit resets.
          )))
(t (princ "you cannot do that"))
  ))


(defun question2()
  (princ "What is the lisp used by UH unix? ")
  (let ((cmd (game-read)))
    (cond ((eq (car cmd) 'allegro)(question3))
          ((equal 6 *attempts2*) (game-over));; checks to see if exceeded guess limit.
          (t (setq *attempts2* (+ *attempts2* 1))(question2));; if not over guess limit resets.

          ))
  )
    
;; third question
(defun question3()
  (princ "What is the airspeed velocity of a unladen swallow? ")
  (let ((cmd (game-read)))
    (cond ((eq (car cmd) 'african_or_european)(princ "I don't know that AHHHHH the old monkey falls through. You hurry to the next floor")
       (setf *location* 'third-floor)
       (setf *Third-Floor* 't)
       (terpri)
       (look))
((equal 6 *attempts2*) (game-over)) ;; checks to see if exceeded guess limit.
(t (setq *attempts2* (+ *attempts2* 1))(question3));; if not over guess limit resets.
          ))
  )

;; game over you missed 6 times you fail and exit lisp
(defun game-over()
  (princ "You fall down a hole and are never seen again")
  (sleep 5)
  (quit)
  )

;;;;;;;;;;;;;;;;;;;;;;;;
; End Brents Functions ;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
; Alvins Functions ;
;;;;;;;;;;;;;;;;;;;;

;;decipher emergency door code.
(defun decipher ()
  (terpri)
  (cond
    ;unlock only on third-floor
    ((AND (eq *location* 'third-floor) (eq *third-floor-solved* NIL))
     (princ "It looks like this emergency door was just installed.")
     (terpri)
     (sleep 2)
     (princ "You examine the door closely and notice a small paper on the floor.")
     (terpri)
     (sleep 2)
     (princ "It looks like instructions on how to unlock the door!")
     (terpri)
     (sleep 2)
     (e-door-manual)
     (set-code)
     (door-puzzle))
    ;checks if third floor is already solved
    ((AND (eq *location* 'third-floor) (eq *third-floor-solved* 't))
      '(emergency door already unlocked.))
    (t
      '(you cannot do that.))))

;;door puzzle loop
(defun door-puzzle ()
  (cond
    ;succuessfull attempt
    ((AND (< *attempts* 16) (eq *e-door-code* *current-guess*))
      (terpri)
      (princ "SUCCESS: DOOR UNLOCKED!")
      (terpri)
      (terpri)      
      (setf *third-floor-solved* 't)
      (setf *location* 'street-level)
      (setf *Street-Level* 't)
      (sleep 1)
      (look))
    ;failed attempt. resets door code
    ((AND (> *attempts* 15) (NOT (eq *e-door-code* *current-guess*)))
     (terpri)
     (princ "FAIL: MAX ATTEMPTS REACHED!")
     (terpri)
     (sleep 1)
     (princ "Resetting System.")
     (terpri)
     (sleep 1)
     (terpri)
     (setf *attempts* 1)
     (setf *current-guess* 0000)
     (look))
    ;else continue on with next attempt
    ((AND (< *attempts* 16) (NOT (eq *e-door-code* *current-guess*)))
      (enter-code))))

;;entering code for door
(defun enter-code ()
  (terpri)
  ;user inputs guess
  (princ "Enter Code: ")
  (let ((guess (read)))
    (setq *current-guess* guess)
    ;checks whether entered guess was correct or not
    (cond
      ((eq *e-door-code* guess)
        (door-puzzle))
      (t
        (princ "Inavlid Code. Try again.")
        (display-stats)
        (setq *attempts* (+ *attempts* 1))
        (door-puzzle)))))

;;sets code for door
(defun set-code ()
  ;generates a random number to determine door code
  (let ((number 1)) ;used to avoid going through game to ease debugging
  ;(let ((number (+ 1 (random 24)))) ;used in real game
  (cond
    ((= number 1)
      (setq *e-door-code* 2379))
    ((= number 2)
      (setq *e-door-code* 2397))
    ((= number 3)
      (setq *e-door-code* 2739))
    ((= number 4)
      (setq *e-door-code* 2793))
    ((= number 5)
      (setq *e-door-code* 2973))
    ((= number 6)
      (setq *e-door-code* 2937))
    ((= number 7)
      (setq *e-door-code* 3279))
    ((= number 8)
      (setq *e-door-code* 3297))
    ((= number 9)
      (setq *e-door-code* 3729))
    ((= number 10)
      (setq *e-door-code* 3792))
    ((= number 11)
      (setq *e-door-code* 3972))
    ((= number 12)
      (setq *e-door-code* 3927))
    ((= number 13)
      (setq *e-door-code* 7329))
    ((= number 14)
      (setq *e-door-code* 7392))
    ((= number 15)
      (setq *e-door-code* 7239))
    ((= number 16)
      (setq *e-door-code* 7293))
    ((= number 17)
      (setq *e-door-code* 7923))
    ((= number 18)
      (setq *e-door-code* 7932))
    ((= number 19)
      (setq *e-door-code* 9372))
    ((= number 20)
      (setq *e-door-code* 9327))
    ((= number 21)
      (setq *e-door-code* 9732))
    ((= number 22)
      (setq *e-door-code* 9723))
    ((= number 23)
      (setq *e-door-code* 9273))
    ((= number 24)
      (setq *e-door-code* 9237)))))

;;prints out door manual
(defun e-door-manual ()
  (terpri)
  (princ " _____________________________________________________________________ ")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "|                   ACME CORPORATION - EMERGENCY DOOR                 |")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "|           ***WARNING - BASIC KNOWLEDGE OF LISP REQUIRED.***         |")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "| *TO UNLOCK:                                                         |")(terpri)
  (princ "|   -ENTER A 4 DIGIT CODE IN THE CORRECT SEQUENCE.                    |")(terpri)
  (princ "|   -EACH INPUT CAN BE FOUND BY ANSWERING QUESTIONS/RIDDLES.          |")(terpri)
  (princ "|   -YOU WILL HAVE 10 TRIES TO GUESS THE CORRECT CODE.                |")(terpri)
  (princ "|   -IF YOU ARE UNABLE TO ENTER THE CORRECT CODE. THE SYSTEM WILL     |")(terpri)
  (princ "|    RESET, ALONG WITH THE CODE.                                      |")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "|  FIRST NUMBER:                                                      |")(terpri)
  (princ "|    I AM AN ODD NUMBER; TAKE AWAY A LETTER AND I BECOME EVEN.        |")(terpri)
  (princ "|    WHAT NUMBER AM I?                                                |")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "|  SECOND NUMBER:                                                     |")(terpri)
  (princ "|    A PROGRAMMER GOES TO A STORE. HIS WIFE TOLD HIM, 'GET A GALLON   |")(terpri)
  (princ "|    OF MILK, AND IF THEY HAVE EGGS 8.' HOW MANY GALLONS OF MILK DID  |")(terpri)
  (princ "|    HE BUY?                                                          |")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "|  THIRD NUMBER:                                                      |")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "|    (setq a 6) (setq b 5) (setq c 1)                                 |")(terpri)
  (princ "|    (defun myster (arg)                                              |")(terpri)
  (princ "|      (cond                                                          |")(terpri)
  (princ "|       ((numberp arg) (incf c) (* arg arg))                          |")(terpri)
  (princ "|       ((stringp arg) (format t ''Happy ~a~%'' arg) arg)             |")(terpri)
  (princ "|       ((and (listp arg) arg) (princ (car arg)) (mystery (cdr arg))) |")(terpri)
  (princ "|       (t 'completetly)))                                            |")(terpri)
  (princ "|    (mystery (+ a b))                                                |")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "|    WHAT IS THE VALUE OF C?                                          |")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "|  FOURTH NUMBER:                                                     |")(terpri)
  (princ "|    (setq even '(0 2 4 6 8))                                         |")(terpri)
  (princ "|    (setq odd '(1 3 5 7 9))                                          |")(terpri)
  (princ "|    (last(reverse(cddr(cdr(reverse(append(reverse even) odd))))))    |")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "|    WHAT DOES THIS RETURN?                                           |")(terpri) 
  (princ "|                                                                     |")(terpri)
  (princ "|_____________________________________________________________________|")(terpri))

;;run function. starts run to airport.
(defun run ()
  (cond
    ;run command only available on street-level
    ((AND (eq *location* 'street-level) (eq *run-turns* 0))
      (terpri)
      (princ "You've made it out of the office.")
      (terpri)
      (sleep 2)
      (princ "Now it's time to make your way back home to the jungle.")
      (terpri)
      (terpri)
      (sleep 2)
      (run-game-instructions)
      (run-game))
    (t
      '(you cannot do that.))))

;;run-game loop
(defun run-game ()
  (cond
    ;successful completion of run-game
    ((AND (= *run-turns* 10) (< *faults* 3))
     (end-game))
    ;failed game. restarts.
    ((= *faults* 3)
     (princ "MISSION FAILED!")
     (terpri)
     (sleep 1)
     (princ "YOU SOMEHOW ENDED UP BACK WHERE YOU STARTED.")
     (terpri)
     (sleep 1)
     (princ "RESTARTING")
     (terpri)
     (fresh-line)
     (setf *run-turns* 0)
     (setf *faults* 0)
     (look))
    ;else, continue through run-game
    (t
     (run-direction)
     (run-game))))

;;Picks a direction for the quick time event(qte).
(defun run-direction ()
  ;generate random number form 1-4 to determine the direction
  (let ((direction (+ 1 (random 4))))
    (cond
      ((= direction 1)
       (qte "= Go NORTH =" 'north "Success!" "Fail. Incorrect input."))
      ((= direction 2)
       (qte "= Go EAST =" 'east "Success!" "Fail. Incorrect input."))
      ((= direction 3)
       (qte "= Go SOUTH =" 'south "Success!" "Fail. Incorrect input."))
      ((= direction 4)
       (qte "= Go WEST =" 'west "Success!" "Fail. Incorrect input."))
      (t
        'north))))

;;quick time event
(defun qte (direction-text success-input success-message fail)
  (princ direction-text)
  (terpri)
  ;set a starting time
  (setq *start-time* (get-internal-real-time))
  ;read in user input
  (princ "Enter direction: ")
  (let ((user-input (read)))
    ;set time it took for user to enter input
    (setq *end-time* (- (get-internal-real-time) *start-time*))
    ;checks whether the user input was correct and within time(< 2 seconds)
    (cond
      ;correct input and within time
      ((AND (eq success-input user-input) (<= *end-time* 2000))
       (princ success-message)
       (setq *run-turns* (+ *run-turns* 1))
       (display-stats))
      ;correct input and over time
      ((AND (eq success-input user-input) (>= *end-time* 2000))
       (princ "Fail. You were too slow.")
       (setq *faults* (+ *faults* 1))
       (display-stats))
      ;wrong input
      (t
       (princ fail)
       (setq *faults* (+ *faults* 1))
       (display-stats)))))

;;shows current amount of run-turns and faults.
(defun display-stats ()
  (cond
    ;during run-game
    ((eq *location* 'street-level)
     (format t "~%Total Turns: ~A~%Total Faults: ~A~%~%" *run-turns* *faults*))
    ;during door-puzzle
    ((eq *location* 'third-floor)
     (format t "~%Total Attempts: ~A~%" *attempts*))))
      

;;instructions for run-game
(defun run-game-instructions ()
  (princ "==============================")
  (terpri)
  (princ "INSTRUCTIONS FOR RUN MINI-GAME")
  (terpri)
  (princ "==============================")
  (terpri)
  (sleep 2)
  (princ "USE YOUR ANIMAL INSTINCTS TO NAVIGATE YOUR WAY TO THE NEAREST CARGO PLANE.")
  (terpri)
  (sleep 2)
  (princ "A DIRECTION WILL BE GENERATED AND YOU WILL HAVE 3 SECONDS TO TYPE IN THAT DIRECTION.")
  (terpri)
  (princ "DIRECTIONS: WEST  EAST  NORTH  SOUTH")
  (terpri)
  (sleep 2)
  (princ "YOU MUST CORRECTLY ENTER THE DIRECTION 10 TIMES TO SUCCEED.")
  (terpri)
  (sleep 3)
  (princ "HOWEVER IF YOU FAIL TO ENTER THE CORRECT DIRECTION 3 TIMES. YOU WILL SENT BACK TO THE BEGINNING.")
  (terpri)
  (sleep 2)
  ;ready prompt for player
  (princ "READY?")
  (terpri)
  (sleep 1)
  (princ "3...")
  (terpri)
  (sleep 1)
  (princ "2..")
  (terpri)
  (sleep 1)
  (princ "1.")
  (terpri)
  (sleep 1)
  (princ "GO!")
  (terpri)
  (sleep 1))

;;ending
(defun end-game ()
  (setf *location* 'jungle)
  (setf *Jungle* t)
  (terpri)
  (princ "== CONGRATULATIONS!  ==")
  (terpri)
  (terpri)
  (sleep 1)
  (princ "YOU HAVE MADE IT SAFELY INTO A CARGO PLANE HEADING STRAIGHT TO YOUR HOME!")
  (terpri)
  (terpri)
  (sleep 1)
  (look))

;;;;;;;;;;;;;;;;;;;;;;;;
; End Alvins Functions ;
;;;;;;;;;;;;;;;;;;;;;;;;
