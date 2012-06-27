; Solutions to exercises 1.21 to 1.28 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at 
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

(define (square x) (* x x))

; Exercise 1.21.
;
; Primality Test. Naive iteration until sqrt(n).
;
(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((= (remainder n test-divisor) 0) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))
;
(smallest-divisor 199) ; 199
(smallest-divisor 1999) ; 1999
(smallest-divisor 19999) ; 7

; Exercise 1.22.
;
; timed-prime-test, when called with integer n, prints n and checks to see if n
; is prime. If n is prime, it prints the time taken to perform the primality test.
;
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
;
; Check the primality of consecutive odd integers within a specified range.
;
(define (search-for-primes start end)
  (cond
    ((even? start) (search-for-primes-helper (+ start 1) end '()))
    (else (search-for-primes-helper start end '()))))

(define (search-for-primes-helper start end primes)
  (cond
    ((> start end) primes)
    ((prime? start) (search-for-primes-helper (+ start 2) end (cons start primes)))
    (else (search-for-primes-helper (+ start 2) end primes))))
;
(search-for-primes 1000 1020) ; (1019 1013 1009)
(search-for-primes 10000 10038) ; (10037 10009 10007)
(search-for-primes 100000 100045) ; (100043 100019 100003)
;
; Runtime.
;
(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
;
; All the above tests returned 0 seconds. Ah! SICP was written in the 80's.
;
; timed-prime-tests repeats the same test count times if n is prime.
;
(define (timed-prime-tests n count)
  (start-timed-prime-tests n count (runtime)))

(define (start-timed-prime-tests n count start-time)
  (cond
    ((= count 0) (report-prime (- (runtime) start-time)))
    ((prime? n) (start-timed-prime-tests n (- count 1) start-time))
    (else #f)))
; 
; Runtime over 1000 iterations.
;
(timed-prime-tests 1009 1000) ; .0600000000000005
(timed-prime-tests 1013 1000) ; .04999999999999982
(timed-prime-tests 1019 1000) ; .04999999999999982
(timed-prime-tests 10007 1000) ; .16000000000000014
(timed-prime-tests 10009 1000) ; .16999999999999993
(timed-prime-tests 10037 1000) ; .16000000000000014
(timed-prime-tests 100003 1000) ; .5299999999999994
(timed-prime-tests 100019 1000) ; .5
(timed-prime-tests 100043 1000) ; .5200000000000005
; 
; Approximately, runtime for 10007 = sqrt(10) x runtime for 1009
; and, runtime for 100003 = sqrt(10) x runtime for 10007.
; The complexity of the algorithm is O(sqrt(n)).
;

; Exercise 1.23.
;
; smallest-divisor modified to ignore all even numbers.
;
(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((= (remainder n test-divisor) 0) test-divisor)
    (else (find-divisor n (next test-divisor)))))

(define (next x)
  (if (= x 2) 3 (+ x 2)))
;
; Runtime for 1000 iterations.
;
(timed-prime-tests 1009 1000) ; 4.9999999999998934e-2 
(timed-prime-tests 1013 1000) ; 4.0000000000000924e-2
(timed-prime-tests 1019 1000) ; .02999999999999936
(timed-prime-tests 10007 1000) ; .10999999999999943
(timed-prime-tests 10009 1000) ; .11000000000000121
(timed-prime-tests 10037 1000) ; .10999999999999943
(timed-prime-tests 100003 1000) ; .33000000000000007
(timed-prime-tests 100019 1000) ; .3200000000000003
(timed-prime-tests 100043 1000) ; .33999999999999986
;
; The runtime is approximately half.
;

; Exercise 1.24.
;
; Fermat test.
;
; expmod calculates (base ^ expo) mod m.
;
(define (expmod base expo m)
  (cond
    ((= expo 0) 1)
    ((even? expo) (remainder (square (expmod base (/ expo 2) m)) m))
    (else (remainder (* (expmod base (- expo 1) m) base) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond 
    ((= times 0) #t)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else #f)))
; 
; Modifying timed-prime-test to use fermat.
;
(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
    (report-prime (- (runtime) start-time))))
; 
; Runtime.
;
(timed-prime-test 1009) ; .03999999999999915
(timed-prime-test 1013) ; 3.0000000000001137e-2
(timed-prime-test 1019) ; .03999999999999915
(timed-prime-test 10007) ; .03999999999999915
(timed-prime-test 10009) ; .05000000000000071
(timed-prime-test 10037) ; .03999999999999915
(timed-prime-test 100003) ; 6.0000000000002274e-2
(timed-prime-test 100019) ; .05999999999999872
(timed-prime-test 100043) ; .05000000000000071

; Exercise 1.25.
;
; Since mod is taken only after calculating (base ^ exp), the number will 
; overflow.
;

; Exercise 1.26.
;
; expmod procedure is called twice at every internal node, making it a tree
; recursion instead of a linear recursion. Every internal node has two branches.
; The number of nodes at level l is (2 ^ l). The height of the tree is (log n).
; Hence, the number of leaves is (2 ^ (log n)) = n.
; The complexity is O(n).
;

; Exercise 1.27.
; 
; Carmichael numbers are those that fool Fermat test. They are not prime,
; but they obey Fermat theorem for all a < n.
;
(define (carmichael-test n)
  (carmichael-test-helper 1 n))

(define (carmichael-test-helper a n)
  (cond
    ((= a n) #t)
    ((= (expmod a n n) a) (carmichael-test-helper (+ a 1) n))
    (else #f)))
;
(carmichael-test 561)
(carmichael-test 1105)
(carmichael-test 1729)
(carmichael-test 2465)
(carmichael-test 2821)
(carmichael-test 6601) 

; Exercise 1.28.
;
; Miller-Rabin primality test.
;
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    ((= times 0) #t)
    ((miller-rabin-test n) (fast-prime? n (- times 1)))
    (else #f)))
;
; If expmod finds a non-trivial square root of 1 mod m, it returns 0.
; Otherwise, it returns (base ^ expo) mod m. 
;
(define (expmod base expo m)
  (define (check-non-trivial-root root)
    (cond
      ((and (not (= root 1)) (not (= root (- m 1))) (= (remainder (square root) m) 1)) 0)
      (else (remainder (square root) m))))
  (cond
    ((= expo 0) 1)
    ((even? expo) (check-non-trivial-root (expmod base (/ expo 2) m)))
    (else (remainder (* base (expmod base (- expo 1) m)) m))))
