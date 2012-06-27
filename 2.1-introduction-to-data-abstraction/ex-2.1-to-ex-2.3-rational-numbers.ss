; Solutions to exercises 2.1 to 2.3 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at 
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-14.html#%_sec_2.1
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Exercise 2.1
;
; Define make-rat
;
; Abstraction for rational numbers using pairs.
;
(define (make-rat n d)
  (define (get-sign n d)
    (cond
      ((and (< n 0) (< d 0)) +)
      ((< n 0) -)
      ((< d 0) -)
      (else +)))
  (let ((g (gcd n d)))
    (cons ((get-sign n d) (abs (/ n g)))
          (abs (/ d g)))))
;
(define (numer x) (car x))
(define (denom x) (cdr x))
;
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Exercise 2.2
;
; Abstraction for line-segments.
;
(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))
(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment)) (x-point (end-segment segment)))
              (average (y-point (start-segment segment)) (y-point (end-segment segment)))))
(define (average x y)
  (/ (+ x y) 2.0))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Exercise 2.3
;
; Rectangle area and perimeter procedures, abstracted from the
; rectangle implementation.
;
(define (rect-area rect)
  (* (rect-length rect) (rect-breadth rect)))
(define (rect-perimeter rect)
  (* 2 (+ (rect-length rect) (rect-breadth rect))))
;
; Rectangle implementation with opposite points.
;
(define (make-rect point-a point-b)
  (cons point-a point-b))
(define (rect-length rect)
  (abs (- (y-point (car rect)) (y-point (cdr rect)))))
(define (rect-breadth rect)
  (abs (- (x-point (car rect)) (x-point (cdr rect)))))
;
; Rectangle implementation with a diagonal.
;
(define (make-rect point-a point-b)
  (make-segment point-a point-b))
(define (rect-length rect)
  (abs (- (y-point (start-segment rect)) (y-point (end-segment rect)))))
(define (rect-breadth rect)
  (abs (- (x-point (start-segment rect)) (x-point (end-segment rect)))))
