; Solutions to exercises 2.33 to 2.43 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Sequences as Conventional Interfaces

; Define accumulate
;
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

; Exercise 2.33.
;
; Basic list-manipulation operations in terms of accumulations.
;
(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
;
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
;
(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; Exercise 2.34.
;
; Evaluating a polynomial using Horner's rule. 
; coefficient-sequence is a list of the co-efficients, 
; arranged from a0 through an.
;
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
;
; Computing 1 + 3x + 5x^3 + x^5 at x = 2
;
(horner-eval 2 '(1 3 0 5 0 1)) ; 79

; Exercise 2.35.
;
; count-leaves from section 2.2.2 as an accumulation.
;
(define (count-leaves tree)
  (accumulate (lambda (x y)
                (if (not (pair? x))
                    (+ 1 y)
                    (+ (count-leaves x) y)))
              0
              tree))
;
; Different implementation using map.
;
(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (x)
                     (cond
                       ((null? x) 0)
                       ((not (pair? x)) 1)
                       (else (count-leaves x))))
                   tree)))

; Exercise 2.36.
;
; Define accumulate-n
;   Takes a sequence of sequences, all with the same length,
;   and combines the ith elements of all the sequences together,
;   and returns a sequence of the results.
;
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init 
              (accumulate (lambda (x y) (cons (car x) y)) '() seqs))
            (accumulate-n op init
              (accumulate (lambda (x y) (cons (cdr x) y)) '() seqs)))))
;
; Simpler implementation using map.
;
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
;
(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) ; (22 26 30)

; Exercise 2.37.
;
; Basic matrix and vector operations using accumulate.
;
; Dot-product of two vectors.
;
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
;
; Product of a matrix and a vector.
;
(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v))
       m))
;
; Transpose of a matrix.
;
(define (transpose mat)
  (accumulate-n cons '() mat))
;
; Product of two matrices.
;
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row))
         m)))

; Exercise 2.38.
;
; Define fold-left
;   Accumulate is fold-right, as it combines the first element of
;   the sequence with the result of combining all the elements to
;   the right. fold-left combines the elements in the opposite direction.
;
(define (my-fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))
;
(fold-right / 1 '(1 2 3)) ; 3/2
(my-fold-left / 1 '(1 2 3)) ; 1/6
(fold-right list '() '(1 2 3)) ; (1 (2 (3 ())))
(my-fold-left list '() '(1 2 3)) ; (((() 1) 2) 3)
;
; If op is associative, then fold-right and fold-left will produce
; the same values for any sequence.

; Exercise 2.39.
;
; Reverse in terms of fold-right and fold-left.
;
(define (my-reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
;
(define (my-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
