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