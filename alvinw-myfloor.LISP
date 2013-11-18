;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name:Alvin Wang, Brent Yoshida, Taylor Kennedy   Date: 10/23/13
;;; Course: ICS313        Assignment: 5 
;;; File: alvinw5.lisp


;;global constant containing name
(defparameter +ID+ "Alvin Wang, Brent Yoshida, and Taylor Kennedy")

;parameters for door-puzzle
(defparameter *third-floor-solved* NIL)
(defparameter *e-door-code* 9999)
(defparameter *current-guess* 0000)
(defparameter *attempts* 1)

;parameters for run-game
(defparameter *run-turns* 0)
(defparameter *faults* 0)
(defparameter *start-time* NIL)
(defparameter *end-time* NIL)

;;variables for description of in-game locations
(defparameter *nodes* '((third-floor (you are on the third floor. you see a locked emergency exit door.))
                        (break-room (you are in the break-room. it is empty.))
                        (street-level (you make your way down the emergency stairs. you made it out of the office building. better make a run for it.))
                        (jungle (you have made it back home to the jungle! Game over. type -quit- to exit.))))

;;describes location
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

;;variables to show connections between locations
(defparameter *edges* '((third-floor (break-room west door))
                        (break-room  (third-floor east door))))

;;desribes connecting edges
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;;used to describe multiple edges
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;list of visible objects
(defparameter *objects* '())

;;list of visible objects and where they are located
(defparameter *object-locations* '())

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

;;variable used to track plater's current position, default location at living room
(defparameter *location* 'third-floor)

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

;;unlock function. takes a door and unlocks it.
(defun unlock ()
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
    ((AND (eq *location* 'third-floor) (eq *third-floor-solved* t))
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
      (push '(street-level north door) (cdr (assoc *location* *edges*)))
      (setf *third-floor-solved* t)
      (rplaca (cdr (assoc 'third-floor *nodes*)) '(you are on the third-floor. the emergency door has been unlocked.))
      (pushnew 'run *allowed-commands*)
      (sleep 1)
      (look))
    ;failed attempt. resets door code
    ((AND (> *attempts* 15) (NOT (eq *e-door-code* *current-guess*)))
     (setf *repeat-third-floor* t)
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
  (setq *location* 'jungle)
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

;;Game REPL for easy input/output
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
          (cond
            ((or (eq (car cmd) 'help) (eq (car cmd) 'h) (eq (car cmd) '?))
             (help-menu)
             (game-repl))
            (t
            (look)
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
  (princ "unlock - Unlocks a door if there is a locked door around.")
  (terpri)
  (princ "help/h/? - Opens the help and hints menu.")
  (terpri)
  (princ "quit - Quit game.")
  (terpri)
  (terpri)
  (princ "Special commands:")
  (terpri)
  (princ "run - Once you have made it out of the office, you can now run away!")
  (terpri))

;;custom read function that concatenates parenthese around commands and a quote infront of parameters
(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;;variable of allowed commands
(defparameter *allowed-commands* '(look walk unlock))

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