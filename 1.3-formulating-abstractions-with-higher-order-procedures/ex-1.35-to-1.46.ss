; Solutions to exercises 1.35 to 1.46 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at 
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_sec_1.3.
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Exercise 1.35.
;
; The transformation x = 1 + 1/x is the equation x^2 - x - 1 = 0, whose root is
; the golden ratio.
;
; Computing golden ratio using fixed-point procedure.
;
(define tolerance 0.00001)

(define (fixed-point f initial-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try initial-guess))

(define golden-ratio 
  (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1.0)) ; 1.6180327868852458 = (1 + sqrt(5))/2, which is the golden ratio.

; Exercise 1.36.
;
; fixed-point that also prints the sequence of approximations.
;
(define (fixed-point f initial-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (report-and-continue guess)
    (newline)
    (display "approx. ")
    (display guess)
    (try guess))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (report-and-continue next))))
  (try initial-guess))
;
; Solution to x^x = 1000 by finding a fixed point of x = log (1000) / log(x) without average damping.
;
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0) ; 4.555532270803653, in 34 steps.
;
; Solution to x^x = 1000 with average damping.
;
(define (average x y) (/ (+ x y) 2.0))

(fixed-point 
  (lambda (x)
    (average x (/ (log 1000) (log x)))) 
  2.0) ; 4.555537551999825, in 9 steps.

; Exercise 1.37.
;
; k-term finite continued fraction (Recursive process).
;
(define (cont-frac n d k)
  (define (do-it i)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (do-it (+ i 1))))))
  (do-it 1))
;
; If all n's and d's are 1, it is an approximation of 1 / golden-ratio.
;
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100) ; .6180339887498948, which is the inverse of golden-ratio.
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10) ; .6179775280898876
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11) ; .6180555555555556
(/ 1.0 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)) ; 1.6179775280898876, which is the golden-ratio.
;
; The min k to get an approximation of 4 decimal places is 11.
;
; k-term finite continued fraction (Iterative process).
;
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

; Exercise 1.38.
;
; Approximation of e based on Euler's expansion.
; Continued fraction expansion for e-2 has all n's as 1, and d's are successively
; 1, 2, 1, 1, 4, 1, 1, 6 1, 1, 8, ...
;
(define euler-n
  (lambda (i) i))

(define euler-d
  (lambda (i)
    (let ((rem (remainder i 3)))
      (cond
        ((or (= rem 0) (= rem 1)) 1)
        (else (* 2 (/ (+ i 1) 3)))))))

(+ 2.0 (cont-frac euler-n euler-d 100)) ; 2.598171741866604, which is e.

; Exercise 1.39.
;
; Approximation to tan based on Lambert's formula.
;
(define (tan-cf x k)
  (let ((neg-x-square (- (* x x)))) 
    (define (tan-n i) (if (= i 1) x neg-x-square))
    (define (tan-d i) (- (* 2 i) 1))
    (cont-frac tan-n tan-d k)))

; Average damping
;
(define (average-damp f)
  (lambda (x) (average x (f x))))

; Square root using average-damp
;
(define (square-root x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

; Cube root using average-damp
;
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

; Newton's method
;
; To solve for g(x) = 0, apply x(n+1) = x(n) - g(x(n)) / Dg(x(n+1)) until convergence.
; This is the same as finding the fixed point of f(x), where f(x) = x - g(x) / Dg(x).
; (Dg(x) = first derivative of g(x))
;
; Procedure that takes a function g and returns the function which is the first derivative 
; of g.
; Dg(x) = (g(x+dx) - g(x)) / dx
;
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))
;
; Procedure for Newton's method
;
(define (newton-transform g)
  (lambda (x) 
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g initial-guess)
  (fixed-point (newton-transform g) initial-guess))
;
; Calculating square root using Newton's method
;
; sqrt(x) is the solution (y) for the equation y^2 - x = 0.
; Here, g(y) = y^2 - x.
;
(define (square-root x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

; Further abstraction for fixed-point
;
(define (fixed-point-of-transform g transform initial-guess)
  (fixed-point (transform g) initial-guess))
;
; Average damp square root using fixed-point-of-transform
;
(define (square-root x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))
;
; Newton's method square root using fixed-point-of-transform
;
(define (square-root x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))

; Exercise 1.40.
;
; Newton's method to solve for x^3 + ax^2 + bx + c = 0.
;
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))
(newtons-method (cubic 1.0 1.0 1.0) 1)

; Exercise 1.41.
;
; Procedure double takes a procedure of one argument as argument and returns
; a procedure that applies the original procedure twice.
;
(define (double f)
  (lambda (x) (f (f x))))
;
(define (inc x) (+ x 1))
((double inc) 1) ; 3
(((double (double double)) inc) 5) ; 21

; Exercise 1.42.
;
; Procedure that implements function composition.
;
(define (compose f g)
  (lambda (x) (f (g x))))
; 
((compose square inc) 6) ; 49

; Exercise 1.43.
;
; Procedure that repeatedly applies a function f a specified number of times.
; 
; Recursive process
;
(define (repeated f n)
  (cond
    ((= n 1) f)
    (else (compose f (repeated f (- n 1))))))
; 
; Iterative process
;
(define (repeated f n)
  (define (iter result i)
    (if (= i n)
      result
      (iter (compose f result) (+ i 1))))
  (iter f 1))
;
((repeated square 2) 5) ; 625

; Exercise 1.44.
;
; Function smoothing
;
; Takes a procedure that computes f and returns a procedure that computes
; the smoothed f.
;
(define dx 0.00001)
(define (smooth f)
  (lambda (x) 
    (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
       3.0)))
;
; n-fold smoothed function - Repeatedly smooth a function n times.
;
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; Exercise 1.45.
; 
(define (pow base expo)
  (define (iter base expo result)
    (cond
      ((= expo 0) result)
      ((even? expo) (iter (square base) (/ expo 2) result))
      (else (iter base (- expo 1) (* base result)))))
  (iter base expo 1.0))
;
; root calculates the n-th root of x.
;
; calc-damps takes n as the argument and returns the minimum number of 
; average damps required for convergence. 
; Based on experiments, it seems that if 2^k <= n < 2^(k+1), then k 
; average damps are required for convergence.
; That is, the number of average damps for n-th root = floor(log2 n).
;
;
(define (root x n)
  (define (calc-damps n) (floor (/ (log n) (log 2.0))))
  (fixed-point-of-transform
    (lambda (y) (/ x (pow y (- n 1))))
    (repeated average-damp (calc-damps n))
    1.0))

; Exercise 1.46.
;
; Abstracting the iterative improvement strategy.
;
(define (iterative-improve good-enough? improve)
  (lambda (initial-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (good-enough? guess next)
          next
          (try next))))
    (try initial-guess)))
;
; sqrt of section 1.1.7 using iterative-improve
;
(define (square-root x)
  (define (good-enough? old-guess new-guess)
    (< (/ (abs (- new-guess old-guess)) new-guess) 0.0001))
  (define (improve guess) (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))
;
; fixed-point of section 1.3.3 in terms of iterative-improve.
;
(define tolerance 0.00001)
(define (fixed-point f initial-guess)
  (define (good-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve good-enough? f) initial-guess))
