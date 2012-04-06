; Solutions to exercises 2.4 to 2.6 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at 
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-14.html#%_sec_2.1
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Exercise 2.4.
;
; Alternative procedural representation of pairs.
;
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

; Exercise 2.5.
;
; Representation of the pair a and b as the (2^a).(3^b)
;
(define (cons-new a b)
  (* (expt 2 a) (expt 3 b)))
(define (car-new x)
  (define (iter count y)
    (if (= (remainder y 2) 0)
        (iter (1+ count) (/ y 2))
        count))
  (iter 0 x))
(define (cdr-new x)
  (define (iter count y)
    (if (= (remainder y 3) 0)
        (iter (1+ count) (/ y 3))
        count))
  (iter 0 x))

; Exercise 2.6.
;
; Church numerals:
;   A representation of numbers using procedures.
;
(define zero 
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one 
  (lambda (f) (lambda (x) (f x))))

(define two 
  (lambda (f) (lambda (x) (f (f x)))))

(define (add n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))
