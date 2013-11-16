;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name:Alvin Wang, Brent Yoshida, Taylor Kennedy   Date: 10/23/13
;;; Course: ICS313        Assignment: 5 
;;; File: alvinw5.lisp


;;global constant containing name
(defparameter +ID+ "Alvin Wang, Brent Yoshida, and Taylor Kennedy")

(defparameter *third-floor-solved* NIL)
(defparameter *e-door-code* 9999)
(defparameter *current-guess* 0000)
(defparameter *attempts* 0)

;;variables for description of in-game locations
(defparameter *nodes* '((third-floor (you are on the third floor. you see a locked emergency exit door.))
                        (break-room (you are in the break-room. it is empty.))
                        (street-level (you make your way down the emergency stairs. you made it out of the office building.))))

;;describes location
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

;;variables to show connections between locations
(defparameter *edges* '((third-floor (break-room west door))
                        (break-room  (third-floor east door))
                        (street-level (third-floor up ladder))))

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
    ((AND (eq *location* 'third-floor) (eq *third-floor-solved* t))
      '(emergency door already unlocked.))
     (t
       '(you cannot do that.))))

;;door puzzle loop
(defun door-puzzle ()
  (cond
    ((AND (< *attempts* 16) (eq *e-door-code* *current-guess*))
      (princ "Door unlocked!")
      (terpri)
      (push '(street-level north door) (cdr (assoc *location* *edges*)))
      (setf *third-floor-solved* t)
      (rplaca (cdr (assoc 'third-floor *nodes*)) '(you are on the third-floor. the emergency door has been unlocked.))
      (look))
    ((AND (> *attempts* 15) (NOT (eq *e-door-code* *current-guess*)))
     (princ "Max attempts reached!")
     (terpri)
     (princ "Resetting System.")
     (terpri)
     (setf *first-time* NIL)
     (setf *attempts* 0)
     (setf *current-guess* 0000)
     (unlock))
    ((AND (< *attempts* 16) (NOT (eq *e-door-code* *current-guess*)))
      (enter-code)
      (door-puzzle))))

;;entering code for door
(defun enter-code ()
  (format t "Enter Code:")
  (let ((guess (read)))
    (setq *current-guess* guess)
    (cond
      ((eq *e-door-code* guess)
        (door-puzzle))
      ((NOT (eq *e-door-code* guess))
        (format t "Inavlid Code. Try again.")
        (fresh-line)
        (setq *attempts* (+ *attempts* 1))
        (door-puzzle)))))

;;sets code for door
(defun set-code ()
  (setq random 1)
  (if (= random 1)
    (setq *e-door-code* 1234)))

;;prints out door manual
(defun e-door-manual ()
  (fresh-line)
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
  (princ "|    (mystery (quote (I Love Lisp)))                                  |")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "|    WHAT IS THE VALUE OF C?                                          |")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "|  FOURTH NUMBER:                                                     |")(terpri)
  (princ "|    (setq even '(0 2 4 6 8))                                         |")(terpri)
  (princ "|    (setq odd '(1 3 5 7 9))                                          |")(terpri)
  (princ "|    ((car(nreverse((cdddr(cons((nreverse x) y)))))))                 |")(terpri)
  (princ "|    WHAT DOES THIS RETURN?                                           |")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "|                                                                     |")(terpri)
  (princ "|_____________________________________________________________________|")(terpri))

;  wizards_game part 2

;;used to start game and initialize a custom REPL
;(defun game-repl ()
    ;(let ((cmd (game-read)))
        ;(unless (eq (car cmd) 'quit)
            ;(game-print (game-eval cmd))
            ;(game-repl))))

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
  (princ "Special commands:")
  (terpri)
  (princ "weld <object> <object> - Welds one object to another.")
  (terpri)
  (princ "dunk <object> <object> - Dunk an object into another.")
  (terpri)
  (princ "splash <object> <object> - Splash an object onto another.")
  (terpri)
  (princ "forge <object> <object> <object> <object> - Forge four objects into one.")
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