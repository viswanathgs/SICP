; Solutions to exercises 1.6 to 1.8 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at 
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Newton's method of successive approximations for calculating square root.
;
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; Terminate if abs(guess*guess - x) < 0.001
;
(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.001))

; Exercise 1.6. 
;
; Alyssa P. Hacker doesn't see why if needs to be provided as a special form. 
; "Why can't I just define it as an ordinary procedure in terms of cond?" she asks. Alyssa's 
; friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:
;
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
    (else else-clause)))
;
; Eva demonstrates the program for Alyssa:
;
(new-if (= 2 3) 0 5) ; 5
(new-if (= 1 1) 0 5) ; 0
;
; Delighted, Alyssa uses new-if to rewrite the square-root program:
;
(define (sqrt-iter-new-if guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter-new-if (improve guess x) x)))
;
; What happens when Alyssa attempts to use this to compute square roots? Explain.
;
; new-if is a procedure and hence the arguments are first evaluated before
; applying them to the procedure (applicative-order evaluation). This results
; in sqrt-iter-new-if getting called recursively with different values for guess
; without ever checking the predicate, leading to an infinite loop.
;

; Exercise 1.7.
;
; good-enough? for small numbers.
;
(square (sqrt 0.0005)) ; 1.325348019146404e-3
(sqrt (square 0.0005)) ; .03125266401721204
;
; For very small values, the precision of 0.001 in good-enough? procedure is
; insufficient.
;
; good-enough? for large values.
;
(sqrt 1e13) ; Never terminates
; 
; The procedure does not terminate for 1e13. For large numbers, after a certain point,
; improve returns the same guess (due to precision issues), and if the tolerance 
; predicate does not succeed with that guess, the procedure enters an infinite
; loop, never actually improving the guess.
;
; New good-enough? that stops when the change in guess is a very small
; fraction of guess.
;
(define (good-enough? guess old-guess)
  (< (abs (- guess old-guess)) (* 0.001 guess)))

(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
    guess
    (sqrt-iter (improve guess x) guess x)))

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))
;
(sqrt (square 0.0005)) ; 5.000000766511453e-4
(square (sqrt 0.0005)) ; 5.000000002265612e-4
(sqrt 1e13) ; 3162277.6640104805
;
; This new good-enough? applies different tolerances for different 
; numbers as the tolerance depends on the new guess and the change in guess.
; Hence, this works well for both large and small values.
;

; Exercise 1.8.
;
; Newton's method for cube roots.
;
(define (cbrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
    guess
    (cbrt-iter (improve guess x) guess x)))

; Newton's method for cube roots is based on the fact that if y is an approximation
; of the cube root of x, then a better approximation is given by
; ((x / (y*y)) + 2*y) / 3
;
(define (improve y x)
  (/ (+ (/ x (* y y)) (* 2 y)) 3))

(define (cbrt x) 
  (cbrt-iter 1.0 0.0 x))
