; Solutions to exercises 2.17 to 2.23 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Exercise 2.17.
;
; Define last-pair
;   Returns a list containing the last element of a given non-empty list.
;
(define last-pair
  (lambda (lst)
    (if (null? (cdr lst))
        lst
        (last-pair (cdr lst)))))
;
(last-pair (list 23 72 149 34)) ; (34)

; Exercise 2.18.
;
; Define reverse
;   Reverse a list.
;
(define (reverse-list lst)
  (define (iter lst rev-lst)
    (if (null? lst)
        rev-lst
        (iter (cdr lst) (cons (car lst) rev-lst))))
  (iter lst '()))
;
(reverse (list 1 4 9 16 25)) ; (25 16 9 4 1)

; Exercise 2.19.
;
; Rewriting the change-counting program of section 1.2.2.
;
(define (cc amount coin-values)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (no-more? coin-values)) 0)
    (else
      (+ (cc amount
             (except-first-denomination coin-values))
         (cc (- amount
                (first-denomination coin-values))
             coin-values)))))

(define (no-more? coin-values) 
  (null? coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1))
;
(cc 100 us-coins) ; 292
(cc 100 uk-coins) ; 4563
;
; The order of the list coin-values does not affect the answer.
;

; Exercise 2.20.
;
; Using dotted-tail notation.
;   Used to define procedures that take arbitrary number of arguments.
;   A dot before the last formal argument matches it to an arbitrary
;   list of actual arguments.
;
; Define same-parity.
;   Takes one or more integers and returns a list of all integers that
;   have the same even-odd parity as the first argument.
;
(define (same-parity a . l)
  (define (eq-parity? a b)
    (if (even? a) (even? b) (odd? b)))
  (define (recur lst)
    (cond
      ((null? lst) '())
      ((eq-parity? a (car lst)) (cons (car lst) (recur (cdr lst))))
      (else (recur (cdr lst)))))
  (cons a (recur l)))
;
(same-parity 1 2 3 4 5 6 7) ; (1 3 5 7)
(same-parity 2 3 4 5 6 7) ; (2 4 6)

; Mapping over lists
;
; Applies the procedure proc to each element of the list iterms.
;
(define (my-map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (my-map proc (cdr items)))))

; Exercise 2.21.
;
; Define square-list
;  Squares each element in a list.
;
(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))
;
(define (square-list items)
  (my-map (lambda (x) (* x x)) items))
;
(square-list (list 1 2 3 4)) ; (1 4 9 16)

; Exercise 2.22.
;
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items '()))
; 
(square-list (list 1 2 3 4)) ; (16 9 4 1)
;
; The above definition returns the answer list in reverse order
; since the things list is emptied in forward order, with each element
; being cons to the beginning of the answer.
;
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))
;
(square-list (list 1 2 3 4)) ; ((((() . 1) . 4) . 9) . 16)
; 
; Reversing the arguments to cons as in the above definition results
; in a list build as pairs. This is because the second argument to cons
; is not a list, but a number. The result of this cons operation is a pair
; of the first and the second arguments.
;

; Exercise 2.23.
;
; Define for-each
;   Similar to map, but just applies to procedure to each element of the list. 
;   Does not form a list of results.
;
(define (my-for-each proc lst)
  (cond
    ((null? lst) #t)
    (else (proc (car lst))
          (my-for-each (cdr lst)))))
;
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
; 57
; 321
; 88
