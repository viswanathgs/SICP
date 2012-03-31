; Solutions to exercises 1.14 to 1.20 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at 
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Exercise 1.14.
;
; Space complexity is O(n) (n is the amount and the number of types of coins
; is assumed to be a constant.
; Eli Bendersky says the time complexity without memoization is O(n^5) (for
; 5 types of coins, of course). 
; http://eli.thegreenplace.net/2007/06/28/sicp-section-123/
;

; Exercise 1.15.
;
; Sine.
;
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs rad) 0.1))
    rad
    (p (sine (/ rad 3.0)))))
;
(sine 12.15) ; The procedure p is applied 5 times
;
; This is linear recursion. The time complexity and the space complexity are
; both O(log (base 3) n).
;

; Exercise 1.16.
;
; Fast exponentiation - Iterative process.
; Calculates b^n.
; Invariant: answer = b^n * res.
;
(define (square x) (* x x))

(define (fast-expt-iter b n res)
  (cond
    ((= n 0) res)
    ((even? n) (fast-expt-iter (square b) (/ n 2) res))
    (else (fast-expt-iter b (- n 1) (* res b)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

; Exercise 1.17.
;
; Multiplication in logarithmic complexity- Recursive process. 
;
(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (fast-mult a b)
  (cond
    ((= b 0) 0)
    ((even? b) (double (fast-mult a (halve b))))
    (else (+ a (fast-mult a (- b 1))))))

; Exercise 1.18.
;
; Multiplication in logarithmic complexity - Iterative process.
; Invariant: answer =  a*b + res.
;
(define (fast-mult-iter a b res)
  (cond
    ((= b 0) res)
    ((even? b) (fast-mult-iter (double a) (halve b) res))
    (else (fast-mult-iter a (- b 1) (+ res a)))))

(define (fast-mult a b)
  (fast-mult-iter a b 0))

; Exercise 1.19.
;
; Fibonacci using matrix exponentiation.
; 
; Let a = fib(n+1), b = f(n).
; Let matrix M[2x2]  = [[p+q, q], [q, p]]
; For the special case, p = 0, q = 1.
;
; The transformation is given by
; M x transpose([a, b]) = transpose([a, b]).
; That is, a <- ap + aq + bq and b <- aq + bp.
;
; Let M1, M2 be two matrices corresponding to two transformations. 
; If M1 transformation is applied after M2, the result is
; M1 x (M2 x transpose([a, b])).
; This is equivalent to the transformation
; (M1 x M2) x transpose([a, b]), due to associativity.
; Hence, this is the same as applying a single transformation
; M1 x M2.
;
; M x M = [[(p+q)^2 + q^2, 2pq + q^2], [2pq + q^2, p^2 + q^2]]
;
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond
    ((= count 0) b)
    ((even? count)
      (fib-iter a 
                b 
                (+ (square p) (square q))
                (+ (* 2 p q) (square q))
                (/ count 2)))
    (else (fib-iter (+ (* a p) (* a q) (* b q))
                    (+ (* a q) (* b p))
                    p
                    q
                    (- count 1)))))

; Exercise 1.20.
;
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
;
; Normal-order evaluation:
;
; (gcd 206 40) ; a = 206, b = 40
; (gcd 40 (remainder 206 40)) ; a = 40, b = 6, remainder evaluated once to check the predicate (remainder-count = 1)
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40))) ; a = 6, b = 4, remainder-count = 1 + 2
; (gcd (remainder 40 (remainder 206 40)) 
;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) ; a = 4, b = 2, remainder-count = 1 + 2 + 4
; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;                                              ; a = 2, b = 0, remainder-count = 1 + 2 + 4 + 7
; Since b = 0 now, a is returned. Evaluating a requires 4 calls to remainder.
; The total number of times remainder is executed, remainder-count = 1 + 2 + 4 + 7 + 4 = 18.
; (gcd 206 40) = 2.
;
; Applicative-order evaluation:
;
; (gcd 206 40)
; (gcd 40 6) ; remainder-count = 1
; (gcd 6 4) ; remainder-count = 2
; (gcd 4 2) ; remainder-count = 3
; (gcd 2 0) ; remainder-count = 4
; 2
; remainder is executed 4 times here.
; 
