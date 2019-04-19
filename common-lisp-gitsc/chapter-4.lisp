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

