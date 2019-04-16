;;;; Common Lisp: A Gentle Introduction to Symbolic Computation.
;;;; Chapter 3. Exercises.


;;; 3.1. What does (NOT (EQUAL 3 (ABS -3))) evaluate to?
(not (equal 3 (abs -3))) ; nil


;;; 3.2. Write an expression in EVAL notation to add 8 to 12 and divide the result by 2.
(/ (+ 8 12) 2) ; 10


;;; 3.3. You can square a number by multiplying it by itself. Write anexpression in EVAL notation
;;; to add the square of 3 and the square of 4.
(+ (* 3 3) (* 4 4)) ; 25


;;; 3.5. Write definitions for HALF, CUBE, and ONEMOREP using DEFUN.
(defun half (n)
  (/ n 2))

(defun cube (n)
  (* n n n))

(defun onemorep (x y)
  (equal (+ x 1) y))


;;; 3.6. Define a function PYTHAG that takes two inputs, x and y, and returns the square root of
;;; x^2 + y^2. You may recognize this as Pythagoras’s formula for computing the length of the
;;; hypotenuse of a right triangle given the lengths of the other two sides. (PYTHAG 3 4) should
;;; return 5.0.
(defun pythag (x y)
  (sqrt (+ (* x x) (* y y))))


;;; 3.7. Define a function MILES-PER-GALLON that takes three inputs, called
;;; INITIAL-ODOMETER-READING, FINAL-ODOMETER-READING, and GALLONS-CONSUMED, and computes
;;; the number of miles traveled per gallon of gas.
(defun miles-per-gallon (initial-miles final-miles gallons)
  (/ (- final-miles initial-miles) gallons))


;;; 3.9. The following expressions evaluate without any errors. Write down the results.
(cons 5 (list 6 7)) ; (5 6 7)

(cons 5 '(list 6 7)) ; (5 list 6 7)

(list 3 'from 9 'gives (- 9 3)) ; (3 FROM 9 GIVES 6)

(+ (length '(1 foo 2 moo))
   (third '(1 foo 2 moo))) ; 6

(rest '(cons is short for construct)) ; (IS SHORT FOR CONSTRUCT)