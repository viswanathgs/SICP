; Solutions to exercises 1.9 to 1.13 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at 
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Exercise 1.9.
;
(define (inc x) 
  (+ x 1))

(define (dec x)
  (- x 1))

(define (add a b)
  (if (= a 0)
    b
    (inc (add (dec a) b))))
;
; The above add procedure is recursive, and the process that it defines
; is also recursive. When the first argument is 0, there is a chain of 
; 'a' deferred inc operations. This is not tail-recursive.
;
(add 4 5)
;
; (add 4 5)
; (inc (add 3 5))
; (inc (inc (add 2 5)))
; (inc (inc (inc (add 1 5))))
; (inc (inc (inc (inc (add 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
;
(define (add a b)
  (if (= a 0)
    b
    (add (dec a) (inc b))))
;
; The above function is a recursive procedure, but an iterative process.
; It's state at any instant can be completely captured by two variables.
; 
; This procedure is tail-recursive. With tail-recursion, iteration can be 
; expressed using the ordinary procedure call mechanism, and so looping constructs
; are not required. With tail-recursion, the memory consumed does not increase 
; with the number of procedure calls, rather, the entire process is executed in
; constant space.
;
(add 4 5)
;
; (add 4 5)
; (add 3 6)
; (add 2 7)
; (add 1 8)
; (add 0 9)
; 9
;

; Exercise 1.10.
;
; Ackermann's function.
;
(define (A x y)
  (cond
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (A (- x 1) (A x (- y 1))))))
;
(A 1 10) ; 1024
(A 2 4) ; 65536
(A 3 3) ; 65536
;
(define (f n) (A 0 n)) ; 2n
(define (g n) (A 1 n)) ; 0 if n = 0, 2^n if n > 0
(define (h n) (A 2 n)) ; 0 if n = 0, 2^2^2^...^2 (n times) if n > 0
(define (k n) (* 5 n n)) ; 5 * n^2

; Exercise 1.11.
;
; Recursive process.
;
(define (f-rec n)
  (cond
    ((< n 3) n)
    (else (+ (f-rec (- n 1))
             (* 2 (f-rec (- n 2)))
             (* 3 (f-rec (- n 3)))))))
; 
; Iterative process.
;
(define (f-iter count a b c)
  (if (= count 0)
    c
    (f-iter (- count 1) (+ a (* 2 b) (* 3 c)) a b)))

(define (f n)
  (f-iter n 2 1 0))

; Exercise 1.12.
;
; Pascal's triangle (0-based indices).
;
(define (pascal row col)
  (cond
    ((or (< row 0) (< col 0) (< row col)) 0)
    ((= row 0) 1)
    (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

; Exercise 1.13.
;
; Standard proof fibonacci and golden ratio proof.
;
