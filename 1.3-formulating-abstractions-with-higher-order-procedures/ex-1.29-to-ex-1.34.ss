; Solutions to exercises 1.29 to 1.34 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at 
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_sec_1.3.
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Abstraction for summation.
;
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (square x) (* x x))

(define (inc x) (+ x 1))

; Naive integral.
;
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))
;
(integral cube 0 1 0.01) ; .24998750000000042
(integral cube 0 1 0.001) ; .249999875000001

; Exercise 1.29.
;
; Integration using Simpson's Rule. Here, n should be even and increasing
; n increases the accuracy of approximation.
;
(define (integral-simpson f a b n)
  (define (calc-h) (/ (- b a) n))
  (define (calc-y k) (f (+ a (* k (calc-h)))))
  (define (co-eff k) 
    (cond
      ((or (= k 0) (= k n)) 1)
      ((even? k) 2)
      (else 4)))
  (define (term k) (* (co-eff k) (calc-y k)))
  (* (/ (calc-h) 3.0) 
     (sum term 0 inc n)))
;
(integral-simpson cube 0 1 100) ; 0.25
(integral-simpson cube 0 1 1000) ; 0.25

; Abstraction for summation (iterative process).
;
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

; Exercise 1.31.
;
; Abstraction for product (recursive process).
;
(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))
; 
; Abstraction for product (iterative process).
;
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))
;
; Factorial in terms for product-iter.
;
(define (factorial n)
  (define (term i)
    (if (= i 0) 
      1
      i))
  (product-iter term 0 inc n))
;
; Approximation to pi using product-iter.
; pi-product converges to pi/4.
;
(define (pi-product b)
  (define (pi-term a)
    (/ (* a (+ a 2)) (square (+ a 1))))
  (define (pi-next a)
    (+ a 2))
  (product-iter pi-term 2 pi-next b))
;
(* 4.0 (pi-product 1000)) ; 3.1431607055322663

; Exercise 1.32.
;
; accumulate - Abstraction for sum and product (or any other combinator)
; Recursive process.
;
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))
; 
; accumulate (Iterative process).
;
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; Exercise 1.33.
;
; Fitered Accumulate - filter is a predicate that checks if a term has to 
; be accumulated or not.
; 
; Iterative process.
;
(define (filtered-accumulate filter-check combiner null-value term a next b)
  (define (iter a result)
    (cond
      ((> a b) result)
      ((filter-check a) (iter (next a) (combiner result (term a))))
      (else (iter (next a) result)))) 
  (iter a null-value))
;
; Sum of squares of primes from a to b.
;
(define (sum-of-squares-prime a b)
 (filtered-accumulate prime? + 0 square a inc b))
;
; Product of all positive integers less than n and relatively prime to n.
;
(define (product-of-relative-primes n)
  (define (term i) i)
  (define (relative-prime? i) (= (gcd i n) 1))
  (filtered-accumulate relative-prime? * 1 term 1 inc (- n 1)))

; Exercise 1.34.
;
(define (f g)
  (g 2))
(f f)
;
; Here, (f f) call produces (f 2), which in turn calls (2 2). The interpreter produces
; an error.
;
;
