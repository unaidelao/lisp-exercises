;;;; Common Lisp: A Gentle Introduction to Symbolic Computation.
;;;; Chapter 5. Exercises.

;;; 5.1. Rewrite function POOR-STYLE to create a new local variable Q using LET,
;;; instead of using SETF to change P. Call your new function GOOD-STYLE.
(defun poor-style (p)
  (setf p (+ p 5))
  (list 'result 'is p))

(defun good-style (p)
  (let ((q (+ 15 p)))
  (list 'result 'is q)))


;;; 5.2. What is side effect?
; Answer: Side effect is everything a function does except returning a value. For example,
; when a functions prints some text.


;;; 5.3. What is the difference between a local and global variable?
; Anwer: It's scope. Global variables are accesible everywhere; local variables are only
; accesibles whitin the function they are in.


;;; 5.4. Why must SETF be a macro function instead of a regular function?
; Answer: a regular function evaluates it's first argument; macros don't. So, SETF is a macro.


;;; 5.5. Are LET and LET* equivalent when you are only creating one local variable?
; Answer: Yes, indeed. They are not equivalent when creating several local variables: LET
; performs the bindings in parallel, LET* does them sequentially.


;;; 5.6. This keyboard exercise is about dice. We will start with a function to throw one dice and
;;; end up with a program to play craps. Be sure to include a documentation string for each
;;; function you write.

;; a) Write a function THROW-DIcE that returns a random number from 1 to 6, inclusive.
;; Remember that (RANDOM 6) will pick numbers from 0 to 5. THROW-DIE doesn’t need any inputs,
;; so its argument list should be NIL.
(defun throw-dice ()
"Returns a random number from 1 to 6."
  (+ 1 (RANDOM 6)))

;; b) Write a function THROW-DICES that throws two dices and returns a list of two numbers:
;; the value of the first die and the value of the second. We’ll call this list a "throw".
;; For example, (THROW-DICES) might return the throw (3 5), indicating that the first dice
;; was a 3 and the second a 5.
(defun throw-dices ()
"Returns a list with two THROW-DICE results."
  (list (throw-dice) (throw-dice)))

;; c) Throwing thwo ones is called "snake eyes"; two sixes is called "boxcars". Write
;; predicates SNAKE-EYES-P and BOXCARS-P that take a throw as input and return T if the
;; throw is equal to (1 1) or (6 6), respectively.
(defun snake-eyes-p (throw)
  (print throw) ; prints throw result, just for curiosity.
  (equal throw '(1 1)))

(defun boxcars-p (throw)
  (print throw) ; prints throw result, just for curiosity.
  (equal throw '(6 6)))

;; d) In playing craps, the first throw of the dice is crucial. A throw of 7
;; or 11 is an instant win. A throw of 2, 3, or 12 is an instant loss (American casino rules).
;; Write predicates INSTANT-WIN-P and INSTANT-LOSS-P to detect these conditions.
;; Each should take a throw as input.
(defun instant-win-p (throw)
  (cond ((equal (+ (first throw) (second throw)) 7) 'you-win)
        ((equal (+ (first throw) (second throw)) 11) 'you-win)
        (t throw)))

(defun instant-loss-p (throw)
  (cond ((equal (+ (first throw) (second throw)) 2) 'you-lose)
        ((equal (+ (first throw) (second throw)) 3) 'you-lose)
        ((equal (+ (first throw) (second throw)) 12) 'you-lose)
        (t throw)))