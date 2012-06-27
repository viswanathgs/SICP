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

; Nested Mappings.

; Exercise 2.40.
;
; Define unique-pairs
;   Given an integer n, generates the sequence of pairs (i, j)
;   with 1 <= j < i <= n.
;
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (1+ low) high))))
;
(define (flat-map proc seq)
  (accumulate append '() (map proc seq)))
;
(define (unique-pairs n)
  (flat-map (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))
;
(unique-pairs 4) ; ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3))
;
; Define prime-sum-pairs
;   Given n, generates all ordered pairs of distict positive integers
;   i and j, where 1 <= j < i <= n, such that i + j is prime.
;
(define (prime-sum-pairs n)
  (map (lambda (p) (list (car p) (cadr p) (+ (car p) (cadr p))))
       (filter is-sum-prime? (unique-pairs n))))
;
(define (is-sum-prime? ordered-pair)
  (prime? (+ (car ordered-pair) (cadr ordered-pair))))
;
(define (prime? n)
  (define (iter i)
    (cond 
      ((> (* i i) n) #t)
      ((= (remainder n i) 0) #f)
      (else (iter (+ i 1)))))
  (iter 2))
;
(prime-sum-pairs 6) ; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

; Exercise 2.41.
;
; Define triple-sum
;   Generates all the ordered triples (i, j, k) such that 1 <= k < j < i <= n
;   and i + j + k = s.
;
(define (unique-triples n)
  (flat-map (lambda (i) 
              (map (lambda (p) (cons i p)) (unique-pairs (- i 1))))
            (enumerate-interval 1 n)))
;
(unique-triples 4) ; ((3 2 1) (4 2 1) (4 3 1) (4 3 2))
;
(define (triple-sum n s)
  (filter (eq-sum? s) (unique-triples n)))
;
; eq-sum? returns a procedure that takes a triple and checks if the sum
; of its elements is equal to s. eq-sum? curries s into the returned procedure.
; 
(define (eq-sum? s)
  (lambda (t) 
    (= s (+ (car t) (cadr t) (caddr t)))))

; Exercise 2.42.
;
; N-queens puzzle
; 
; Define queens
;   Returns a list of all possible valid placements for the given board size.
;
; Define queen-cols
;   Returns a list of all possible valid placements for the first k columns
;   of the given board size.
;
; Each placement is represented as a list of length board-size. If the ith
; element of the list is x, then a queen is placed at ith column at xth row.
;
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flat-map
            (lambda (rest-of-queens)
              (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))
;
(define empty-board '())
;
; adjoin-position just appends new-row to rest-of-queens.
; rest-of-queens has the placement for the first k-1 columns.
;
(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))
;
; safe? checks if the kth position is safe with respect to the 
; first k-1 positions.
;
(define (safe? k positions)
  (define (iter col seq new-row)
    (let ((row (car seq)))
      (cond
        ((= col k) #t)
        ((= row new-row) #f)
        ((= (abs (- row new-row)) (abs (- col k))) #f)
        (else (iter (1+ col) (cdr seq) new-row)))))
  (iter 1 positions (get-nth k positions)))
;
; get-nth returns the nth element (1-based) in seq.
;
(define (get-nth n seq)
  (if (= n 1)
      (car seq)
      (get-nth (- n 1) (cdr seq))))
;
(queens 4) ; ((2 4 1 3) (3 1 4 2))

; Exercise 2.43.
; 
; The following queens implementation has interchanged order of the 
; nested mappings iin the flat-map.
;
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flat-map
            (lambda (new-row)
              (map (lambda (rest-of-queens) (adjoin-position new-row k rest-of-queens))
                   (queen-cols (- k 1))))
            (enumerate-interval 1 board-size)))))
  (queen-cols board-size))
;
; Due to the interchange in nested mappings, for every possible value of 
; new-row (from 1 to board-size), (queen-cols (- k 1)) is called. This leads
; to an increase in time by a factor of board-size for each k. Hence, for all 
; values of k, the time is increased by a factor of (board-size ^ board-size).
;
