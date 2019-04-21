;;;; Common Lisp: A Gentle Introduction to Symbolic Computation.
;;;; Chapter 4. Exercises.

;;; 4.1. Write a function MAKE-EVEN that makes an odd number even by adding one to it.
;;; If the input to MAKE-EVEN is already even, it should be returned unchanged.
(defun make-even (x)
  (if (evenp x) x (+ x 1)))


;;; 4.2. Write a function FURTHER that makes a positive number larger by adding one to it,
;;; and a negative number smaller by subtracting one from it.
;;; What does your function do if given the number 0?
(defun further (x)
  (if (> x 0) (+ x 1) (- x 1)))

; Answer: Given the number 0, this function returns -1.


;;; 4.3. Recall the primitive function NOT: It returns NIL for a true input and T for a false one.
;;; Suppose Lisp didn’t have a NOT primitive. Show how to write NOT using just IF and constants
;;; (no other functions). Call your function MY-NOT.
(defun my-not (x)
  (if x nil t))


;;; 4.4. Write a function ORDERED that takes two numbers as input and makes a list of them in
;;; ascending order. (ORDERED 3 4) should return the list (3 4). (ORDERED 4 3) should also return
;;; (3 4), in other words, the first and second inputs should appear in reverse order when the
;;; first is greater than the second.
(defun ordered (x y)
  (if (< x y) (list x y)
      (list y x)))


;;; 4.5. For each of the following calls to COMPARE, write ‘‘1,’’ ‘‘2,’’ or ‘‘3’’ to indicate which
;;; clause of the COND will have a predicate that evaluates to true.
(defun compare (x y)
  (cond ((equal x y) 'numbers-are-the-same)
        ((< x y) 'first-is-smaller)
        ((> x y) 'first-is-bigger)))

;; (compare 9 1) -> Answer: 3
;; (compare (+ 2 2) 5) -> Answer: 2
;; (compare 6 (* 2 3)) -> Answer: 1


;;; 4.6. Write a version of the absolute value function MY-ABS using COND instead of IF.
(defun my-abs-cond (x)
  (cond ((< x 0) (- x))
        (t x)))


;;; 4.7. For each of the following COND expressions, tell whether the parenthesization is correct
;;; or incorrect. If incorrect, explain where the error lies.
(cond (symbolp x) 'symbol
      (t 'not-a-symbol))
; Answer: Parenthesization incorrect.

(cond ((symbolp x) 'symbol)
      (t 'not-a-symbol))
; Answer: Parenthesization correct.

(cond ((symbolp x) ('symbol)
      (t 'not-a-symbol))
; Answer: Parenthesization incorrect.

(cond ((symbolp x) 'symbol)
      ((t 'not-a-symbol)))
; Answer: Parenthesization incorrect.


;;; 4.8. Write EMPHASIZE3, which is like EMPHASIZE2 but adds the symbol VERY onto the list if it
;;; doesn't know how to emphasize it.
;;; For example, EMPHASIZE3 of (LONG DAY) should produce (VERY LONG DAY).
(defun emphasize3 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
        ((equal (first x) 'bad) (cons 'awful (rest x)))
        (t (cons 'very x))))

;;; What does EMPHASIZE3 of (VERY LONG DAY) produce?
; Answer:(EMPHASIZE '(VERY LONG DAY)) -> (VERY VERY LONG DAY)


;;; 4.9. Type in the following suspicious definition:
(defun make-odd (x)
  (cond (t x)
        ((not (oddp x)) (+ x 1))))
;; What is wrong with this function?
; Answer: the clauses are in the wrong order. In this case, because of the first COND clause,
; this function will always return it's input.

;; Try out the function on the numbers 3, 4, and -2.
(make-odd 3) ; 3
(make-odd 4) ; 4
(make-odd -2) ; -2

;; Rewrite it so it works correctly.
(defun make-odd (x)
  (cond ((not (oddp x)) (+ x 1))
        (t x)))


;;; 4.10. Write a function CONSTRAIN that takes three inputs called X, MAX, and MIN.
;;; If X is less than MIN, it should return MIN; if X is greater than MAX, it should return MAX.
;;; Otherwise, since X is between MIN and MAX, it should return X.
;;; (CONSTRAIN 3 50 -50) should return 3.
;;; (CONSTRAIN 92 50 -50) should return 50.
;;; Write one version using COND and another using nested IFs.
(defun constrain-cond (x max min)
  (cond ((< x min) min)
        ((> x max) max)
        (t x)))

(defun constrain-if (x max min)
  (if (< x min) min
      (if (> x max) max
          x)))


;;; 4.11. Write a function FIRSTZERO that takes a list of three numbers as input and returns a word
;;; (one of "first," "second," "third," or "none") indicating where the first zero appears
;;; in the list.
;;; Example: (FIRSTZERO '(3 0 4)) should return SECOND.
(defun firstzero (x)
  (cond ((equal (first x) 0) 'first)
        ((equal (second x) 0) 'second)
        ((equal (third x) 0) 'third)
        (t 'none)))

(defun firstzero2 (x)
  (cond ((zerop (first x)) 'first)
        ((zerop (second x)) 'second)
        ((zerop (third x)) 'third)
        (t 'none)))

;; What happens if you try to call FIRSTZERO with three separate numbers instead of a list of three
;; numbers, as in (FIRSTZERO 3 0 4)?

; Answer: Since FIRSTZERO uses FIRST, SECOND and THIRD built-in functions, FIRSTZERO argument must
; be a list. So, calling FIRSTZERO with three separate numbers will cause a invalid number of
; arguments error.


;;; 4.12. Write a function CYCLE that cyclically counts from 1 to 99.
;;; CYCLE called with an input of 1 should return 2, with an input of 2 should return 3, with an
;;; input of 3 should return 4, and so on.
;;; With an input of 99, CYCLE should return 1. That’s the cyclical part.
;;; Do not try to solve this with 99 COND clauses!
(defun cycle (x)
  (cond ((equal x 99) 1)
        (t (+ x 1))))


;;; 4.13. Write a function HOWCOMPUTE that is the inverse of the COMPUTE function described
;;; previously. HOWCOMPUTE takes three numbers as input and figures out what operation would
;;; produce the third from the first two.
;;;
;;; (HOWCOMPUTE 3 4 7) should return SUM-OF.
;;; (HOWCOMPUTE 3 4 12) should return PRODUCT-OF.
;;; HOWCOMPUTE should return the list (BEATS ME) if it can’t find a relationship between the first
;;; two inputs and the third. Suggest some ways to extend HOWCOMPUTE.
(defun howcompute (a b result)
  (cond ((equal (+ a b) result) 'sum-of)
        ((equal (* a b) result) 'product-of)
        ((equal (- a b) result) 'subtraction-of)
        ((equal (/ a b) result) 'division-of)
        (t '(beats me))))


;;; 4.14. What results do the following expressions produce?
;;; Read the evaluation rules for AND and OR carefully before answering.
(and 'fee 'fie 'foe) ; FOE

(or 'fee 'fie 'foe) ; FEE

(or nil 'foe nil) ; FOE

(and 'fee 'fie nil) ; NIL

(and (equal 'abc 'abc) 'yes) ; YES

(or (equal 'abc 'abc) 'yes) ; T


;;; 4.15. Write a predicate called GEQ that returns T if its first input is greater than or equal
;;; to its second input.
(defun geq (x y)
  (or (> x y)
      (equal x y)))


;;; 4.16. Write a function that squares a number if it is odd and positive, doubles it if it is
;;; odd and negative, and otherwise divides the number by 2.
(defun ex4-16 (x)
  (cond ((and (oddp x) (> x 0)) (* x x))
        ((and (oddp x) (< x 0)) (* x 2))
        (t (/ x 2))))


;;; 4.17. Write a predicate that returns T if the first input is either BOY or GIRL and the
;;; second input is CHILD, or the first input is either MAN or WOMAN and the second input
;;; is ADULT.
(defun ex4-17 (x y)
  (or (and (or (equal x 'boy)
               (equal x 'girl))
           (equal y 'child))
      (and (or (equal x 'man)
               (equal x 'woman))
           (equal y 'adult))))

;;; 4.18. Write a function to act as referee in the Rock-Scissors-Paper game. In this game,
;;; each player picks one of Rock, Scissors, or Paper, and then both players tell what they
;;; picked. Rock "breaks" Scissors, so if the first player picks Rock and the second picks
;;; Scissors, the first player wins. Scissors "cuts" Paper, and Paper "covers" Rock.
;;; If both players pick the same thing, it's a tie. The function PLAY should take two
;;; inputs, each of which is either ROCK, SCISSORS, or PAPER, and return one of the symbols
;;; FIRST-WINS, SECOND-WINS, or TIE.
;;;
;;; Examples: (PLAY 'ROCK 'SCISSORS) should return FIRST-WINS.
;;; (PLAY 'PAPER 'SCISSORS) should return SECOND-WINS.
(defun play (x y)
  (cond ((equal x y) 'tie)
        ((or (and (equal x 'paper)
                  (equal y 'rock))
             (and (equal x 'rock)
                  (equal y 'scissors))
             (and (equal x 'scissors)
                  (equal y 'paper))) 'first-wins)
        (t 'second-wins)))


;;; 4.19. Show how to write the expression (AND X Y Z W) using COND instead of AND.
(cond ((not x) nil)
      ((not y) nil)
      ((not z) nil)
      (t w))

;;;Then show how to write it using nested IFs instead of AND.
(if x
  (if y
    (if z w)))


;;; 4.20. Write a version of the COMPARE function using IF instead of COND.
(defun compare-2 (x y)
  (if (equal x y) 'numbers-are-the-same
      (if (< x y) 'first-is-smaller 'first-is-bigger)))

;;;Also write a version using AND and OR.
(defun compare-3 (x y)
  (or (and (equal x y) 'numbers-are-the-same)
      (and (< x y) 'first-is-smaller)
      'first-is-bigger))


;;; 4.21. Write versions of the GTEST function using IF and COND
(defun gtest-2 (x y)
  (if (> x y) t
      (if (zerop x) t
          (zerop y))))

(defun gtest-3 (x y)
  (cond ((> x y) t)
        ((zerop x) t)
        (t (zerop y))))


;;; 4.22. Use COND to write a predicate BOILINGP that takes two inputs, TEMP and SCALE, and
;;; returns T if the temperature is above the boiling point of water on the specified scale.
;;; If the scale is FAHRENHEIT, the boiling point is 212 degrees; if CELSIUS, the boiling
;;; point is 100 degrees.
;;; Also write versions using IF and AND/OR instead of COND.
(defun boilingp (temp scale)
  (cond ((and (equal scale 'fahrenheit) (> temp 212)) t)
        ((and (equal scale 'celsius) (> temp 100)) t)))

(defun boilingp-2 (temp scale)
  (if (and (equal scale 'fahrenheit) (> temp 212)) t
      (if (and (equal scale 'celsius) (> temp 100)) t nil)))

(defun boilingp-3 (temp scale)
  (or (and (equal scale 'fahrenheit)
           (> temp 212))
      (and (equal scale 'celsius)
           (> temp 100))))