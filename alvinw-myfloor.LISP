;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name:Alvin Wang, Brent Yoshida, Taylor Kennedy   Date: 10/23/13
;;; Course: ICS313        Assignment: 5 
;;; File: alvinw5.lisp


;;global constant containing name
(defparameter +ID+ "Alvin Wang, Brent Yoshida, and Taylor Kennedy")

(defparameter *third-floor-solved* NIL)
(defparameter *second-floor-first* NIL)
(defparameter *second-floor-second* NIL)
(defparameter *second-floor-third* NIL)


;;variables for description of in-game locations
(defparameter *nodes* '((third-floor (you are on the third floor. you see a locked emergency exit door.))
(second-floor (you are on the second floor. you see a long hallway and a emergency exit door at the end an old monkey is standing in front of you and a fellow monkey is standing in front of you. you can "talk" to him. there is a book to your left that reads "african or european" which swallow is faster.))
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
     (door-puzzle))
    ((AND (eq *location* 'third-floor) (eq *third-floor-solved* t))
      '(emergency door already unlocked.))
     (t
       '(you cannot do that.))))

;;unlock function. takes a door and unlocks it.
(defun unlock-second ()
  (cond
    ;unlock only on second-floor
    ((AND (eq *location* 'second-floor) (eq *second-floor-third* NIL))
     (door-puzzle))
    ((AND (eq *location* 'second-floor) (eq *second-floor-third* t))
      '(emergency door already unlocked.))
     (t
       '(you cannot do that.))))


;;door-puzzle
(defun door-puzzle ()
  (format t "Unlocking door...")
  (fresh-line)
  (sleep 3)
  (format t "Door unlocked!")
  (fresh-line)
  (push '(street-level north door) (cdr (assoc *location* *edges*)))
  (setf *third-floor-solved* t)
  (rplaca (cdr (assoc 'third-floor *nodes*)) '(you are on the third-floor. the emergency door has been unlocked.))
  (look))


(defun talk ()
(princ "The old man speaks to the other monkey To pass this way you must answer my questions 3 What is your name? The monkey answers Sir Monkihad. What is your quest. What is your favorite color? Blue no Gre... AHHHHH as the monkey falls through the floor.")
(fresh-line)
(princ "What is the (cdr (cdr (cdr list))) represented as.")
(let ((cmd (game-read)))
 (if(eq (car cmd) 'cdr)(princ "yay"))))


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
(defparameter *allowed-commands* '(look walk unlock talk))

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