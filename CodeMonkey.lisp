;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;; Name:Alvin Wang, Brent Yoshida, Taylor Kennedy   Date: 10/23/13
;;; Course: ICS313        Assignment: 6
;;; File: taylorak5.lisp

;;;;;;;;;;;;;;;;;;
; Global Objects ;
;;;;;;;;;;;;;;;;;;

;;global constant containing name
(defparameter +ID+ "Alvin Wang, Brent Yoshida, and Taylor Kennedy")

;;global constants to check what floors you are currently on
(defparameter *First-Floor* t)
(defparameter *Second-Floor* NIL)
(defparameter *Third-Floor* NIL)
(defparameter *Street-Level* NIL)

;;variables for description of in-game locations
(defparameter *nodes* '((office (you are in an office.
                            a gorilla is sitting at the office desk.
                            The door to the next floor is locked.))
                        (break-room (you are in the break room.
                            there is a blender and freezer in the corner.))
                        (storage (you are in the storage room.
                            there is a mountain of bananas in the middle of the room.))
                        ))

;;variables to show connections between locations
(defparameter *edges* '((office (break-room right hallway))
                        (break-room (office left hallway)
                                    (storage right door))
                        (storage (break-room left door))
                        ))

;;list of visible objects
(defparameter *objects* '(bananas))

;;list of visible objects and where they are located
(defparameter *object-locations* '((bananas storage)))

;;variable used to track plater's current position, default location at living room
(defparameter *location* 'office)

;;variable of allowed commands
(defparameter *allowed-commands* '(look walk pickup inventory))

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
    ((eq *Third-Floor* t)
     (princ "Alvins help section")
     (terpri))
    ((eq *Second-Floor* t)
     (princ "Brents help section")
     (terpri))
    ((eq *First-Floor* t)
     (princ "Special commands:")
     (terpri)
     (princ "blend bananas blender - blends bananas into a smoothie .")
     (terpri)
     (princ "give smoothie gorilla - give a smoothie to the gorilla.")
     (terpri)
     (princ "unlock key door - unlock the door to the next level.")
     (terpri))
    ))

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
(game-action give smoothie gorilla office
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
(game-action unlock key door office
             (if (not (have 'key))
               '(The door requires a key. The gorilla next to you seems like he knows something.
                     He seems to be muttering something about a banana smoothie.)
               (progn (new-path office down elevator storage)
                      (princ "The door is unlocked. You make your way down to the next floor")
                      (walk 'down)
                      (setq *Second-Floor* t))
               ))
		   
;;;;;;;;;;;;;;;;;;;;;;;;;
; End Taylors Functions ;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
; Brents Functions ;
;;;;;;;;;;;;;;;;;;;;

; Brent Add your unique functions here
; Globals go in the global section at top

;;;;;;;;;;;;;;;;;;;;;;;;
; End Brents Functions ;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
; Alvins Functions ;
;;;;;;;;;;;;;;;;;;;;

; Alvin add your unique functions here
; Globals go in the global section at top

;;;;;;;;;;;;;;;;;;;;;;;;
; End Alvins Functions ;
;;;;;;;;;;;;;;;;;;;;;;;;
