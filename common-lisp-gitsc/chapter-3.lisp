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


;;; 3.10. The following expressions all result in errors. Write down the type of error that occurs,
;;; explain how the error arose (for example, missing quote, quote in wrong place), and correct the
;;; expression by changing only the quotes.
(third (the quick brown fox)) ; correct: (third '(the quick brown fox))

(list 2 and 2 is 4) ; correct: (list 2 'and 2 'is 4)

(+ 1 '(length (list t t t t))) ; correct: (+ 1 (length (list t t t t)))

(cons 'patrick (seymour marvin)) ; correct: (cons 'patrick '(seymour marvin))


;;; 3.11. Define a predicate called LONGER-THAN that takes two lists as input and returns T if the
;;; first list is longer than the second.
(defun longer-than (list1 list2)
  (> (length list1) (length list2)))


;;; 3.12. Write a function ADDLENGTH that takes a list as input and returns a new list with the
;;; length of the input added onto the front of it. If the input is (MOO GOO GAI PAN), the output
;;; should be (4 MOO GOO GAI PAN).
;;;
;;; What is the result of (ADDLENGTH (ADDLENGTH '(A B C)))?
(defun addlength (x)
  (cons (length x) x))

;; Result of (ADDLENGTH (ADDLENGTH '(A B C))) -> (4 3 A B C)


;;; 3.13. Study this function definition:
(defun call-up (caller callee)
  (list 'hello callee 'this 'is caller 'calling))

;; How many arguments does this function require?
; Answer: This function requires 2 arguments.

;; What are the names of the arguments?
; Answer: CALLER and CALLEE.

;; What is the result of (CALL-UP 'FRED 'WANDA)?
; Answer: (HELLO WANDA THIS IS FRED CALLING)


;;; 3.14. Here is a variation on the CALL-UP function from the previous problem.
(defun crank-call (caller callee)
  '(hello callee this is caller calling))

;; What is the result of (CRANK-CALL 'WANDA 'FRED)?
; Answer: (HELLO CALLEE THIS IS CALLER CALLING)


;;; 3.15. Consider the following function, paying close attention to the quotes:
(defun scrabble (word)
  (list word 'is 'a 'word))

;; The symbol WORD is used two different ways in this function. What are they?
; Answer: Used without quote, the symbol is used as a variable. With quote, is used as data.

;; What is the result of (SCRABBLE 'AARDVARK)?
; Answer: (AARDVARK IS A WORD)

;; What is the result of (SCRABBLE 'WORD)?
; Answer: (WORD IS A WORD)


;;; 3.16. Here's a real confuser:
(defun stooge (larry moe curly)
  (list larry (list 'moe curly) curly 'larry))

;; What does the following evaluate to?: (stooge 'moe 'curly 'larry)
; Answer: (MOE (MOE LARRY) LARRY LARRY)


;;; 3.17. Why can’t the special symbols T or NIL be used as variables in a function definition?
; Answer: Because T and NIL evaluates themselves, and because they are reserved words in LISP.


;;; 3.19. Evaluate each of the following lists. If the list causes an error, tell what
;;; the error is. Otherwise, write the result of the evaluation.
(cons 'grapes '(of wrath)) ; (GRAPES OF WRATH)

(list t 'is 'not nil) ; (T IS NOT NIL)

(first '(list moose goose)) ; LIST

(first (list 'moose 'goose)) ; MOOSE

(cons 'home ('sweet 'home)) ; ERROR: ILLEGAL FUNCTION CALL 'SWEET


;;; 3.20. Here is a mystery function:
(defun mystery (x)
  (list (second x) (first x)))

;; What result or error is produced by evaluating each of the following?
(mystery '(dancing bear)) ; (BEAR DANCING)

(mystery 'dancing 'bear) ; INVALID NUMBER OF ARGUMENTS: 2

(mystery '(zowie)) ; (NIL ZOWIE)

(mystery (list 'first 'second)) ; (SECOND FIRST)


;;; 3.21. What is wrong with each of the following function definitions?
(defun speak (x y) (list 'all 'x 'is 'y)) ; Answer: Variables X and Y not used. (Don't quote variables!).

(defun speak (x) (y) (list 'all x 'is y)) ; Answer: Variable X not used and Undefined variable Y. (Function must have only one argument list!).

(defun speak ((x) (y)) (list all 'x is 'y)) ; Answer: Required argument is not a symbol (Remove variables parenthesis!). ALL and IS must be quoted, not the variables.


;;; 3.22.
;;;
;; a) Find out how to run Lisp on your computer, and start it up.
; Answer: I use two different options: running SBCL through the CMD (at Windows), or running
; Portacle, which is maybe the fastest and easiest Lisp solution for a beginner.


;; b) For each following expression, write down what you think it evaluates to or what kind of
;; error it will cause. Then try it on the computer and see.
(+ 3 5) ; 8

(3 + 5) ; Error: illegal function call "3"

(+ 3 (5 6)) ; Error: illegal function call "5"

(+ 3 (* 5 6)) ; 33

'(morning noon night) ; (MORNING NOON NIGHT)

('morning 'noon 'night) ; Error: illegal function call "'morning"

(list 'morning 'noon 'night) ; (MORNING NOON NIGHT)

(car nil) ; NIL

(+ 3 foo) ; Error: illegal function call. Unasigned variable FOO

(+ 3 'foo) ; Error: 'foo is not a number.


;; c) Here is an example of the function MYFUN, a strange function of two inputs:
(myfun 'alpha 'beta) ; ((ALPHA) BETA)

;; Write MYFUN. Test your function to make certain it works correctly.
(defun myfun (x y)
  (list (list x) y))


;; d) Write a predicate FIRSTP that returns T if its first argument (a symbol) is equal to the
;; first element of its second argument (a list). That is, (FIRSTP 'FOO '(FOO BAR BAZ)) should
;; return T. (FIRSTP 'BOING '(FOO BAR BAZ)) should return NIL.
(defun firstp (x y)
  (equal x (first y)))


;; e) Write a function MID-ADD1 that adds 1 to the middle element of a three-element list.
;; For example, (MID-ADD1 '(TAKE 2 COOKIES)) should return the list (TAKE 3 COOKIES).
;; Note: You are not allowed to make MID-ADD1 a function of three inputs. It has to take a single
;; input that is a list of three elements.
(defun mid-add1 (x)
  (list (first x) (+ 1 (second x)) (third x)))


;; f) Write a function F-TO-C that converts a temperature from Fahrenheit to Celsius.
;; The formula for doing the conversion is:
;;                 Celsius temperature = [5 x (Fahrenheit temperature - 32)]/9.
;; To go in the opposite direction, the formula is:
;;                 Fahrenheit temperature = (9/5 x Celsius temperature) + 32.
(defun f-to-c (x)
  (/ (* 5 (- x 32.0)) 9))

(defun c-to-f (x)
  (+ (* 9/5 x) 32.0))


;; g) What is wrong with this function? What does (FOO 5) do?
(defun foo (x)
  (+1 (zerop x)))
; Answer 1: It's wrong because zerop return T or NIL, and is trying to add 1 to that returned result.
; Ansert 2: (FOO 5) returns Error.


;;; 3.23. Write each of the following functions in Church’s lambda notation:
;;; a) DOUBLE
;;; b) SQUARE
;;; c) ONEMOREP.
; Answer a: lambda n . n x 2
; Answer b: lambda n . n x n
; Answer c: lambda (x, y) . x = (y + 1)


;;; 3.25. What do each of the following expressions evaluate to?
(list 'cons t nil) ; (CONS T NIL)

(eval (list 'cons t nil)) ; (T)

(eval (eval (list 'cons t nil))) ; Error: T is undefined.

(apply #'cons '(t nil)) ; (T)

(eval nil) ; NIL

(list 'eval nil) ; (EVAL NIL)

(cons 'eval nil) ; (EVAL)

(eval (list 'eval nil)) ; NIL