; Solutions to exercises 2.24 to 2.32 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Exercise 2.24.
;
(list 1 (list 2 (list 3 4))) ; (1 (2 (3 4)))

; Exercise 2.25.
;
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9)))))) ; 7
(car (car '((7)))) ; 7
(cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7)))))))))))) ; 7

; Exercise 2.26.
;
(define x (list 1 2 3))
(define y (list 4 5 6))
;
(append x y) ; (1 2 3 4 5 6)
(cons x y) ; ((1 2 3) 4 5 6)
(list x y) ; ((1 2 3) (4 5 6))

; Exercise 2.27.
;
; Define deep-reverse
;   Recursively reverse sub-lists too.
;
(define (deep-reverse lst)
  (cond
    ((null? lst) '())
    ((not (pair? lst)) lst)
    (else (append (deep-reverse (cdr lst)) (list (deep-reverse (car lst)))))))
;
(deep-reverse '((1 2) (3 4))) ; ((4 3) (2 1))

; Exercise 2.28.
;
; Define fringe.
;   Takes a tree (represented as a list, and returns a list whose
;   elements are all the leaves of the tree, arranged in left-to-right
;   order.
;
(define (fringe lst)
  (cond
    ((null? lst) '())
    ((not (pair? lst)) (list lst))
    (else (append (fringe (car lst)) (fringe (cdr lst))))))
;
(define x '((1 2) (3 4)))
(fringe x) ; (1 2 3 4)
(fringe (list x x)) ; (1 2 3 4 1 2 3 4)

; Exercise 2.29.
;
; Binary mobile.
;
; Binary mobile constructed from its left and right branches.
;
(define (make-mobile left right)
  (list left right))
;
; Branch constructed for its length and a structure.
; Structure can be a number (representing a simple weight), or
; another mobile.
;
(define (make-branch len structure)
  (list len structure))
;
; a. Selectors.
;
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car (branch)))
(define (branch-structure branch)
  (car (cdr branch)))
;
; b. Define total-weight, that returns the total weight of a mobile.
;
(define (total-weight mobile)
  (cond
    ((not (pair? mobile)) mobile)
    (else (+ (total-weight (branch-structure (left-branch mobile)))
             (total-weight (branch-structure (right-branch mobile)))))))
;
; c. Define balanced?, which checks if a binary mobile is balanced.
;
(define (balanced? mobile)
  (cond
    ((not (pair? mobile)) #t)
    ((not (balanced? (branch-structure (left-branch mobile)))) #f)
    ((not (balanced? (branch-structure (right-branch mobile)))) #f)
    (else (= (torque (left-branch mobile))
             (torque (right-branch mobile))))))
;
(define (torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))
;
; d. Different representation of mobiles.
;
(define (make-mobile left right)
  (cons left right))
(define (make-branch len structure)
  (cons len structure))
;
(define (right-branch mobile)
  (cdr mobile))
(define (branch-structure branch)
  (cdr branch))

; Exercise 2.30.
;
; Define square-tree (without using map).
;   Squares each leaf in a tree.
;
(define (square-tree tree)
  (cond
    ((null? tree) '())
    ((not (pair? tree)) (* tree tree))
    (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))
;
; Define square-tree (using map).
;
(define (square-tree tree)
  (map (lambda (sub-tree)
         (cond
           ((not (pair? sub-tree)) (* sub-tree sub-tree))
           (else (square-tree sub-tree))))
       tree))
;
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))) ; (1 (4 (9 16) 25) (36 49))

; Exercise 2.31.
; 
; Abstracting exercise 2.30 using tree-map. This applies a procedure
; proc to each leaf of the tree.
;
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (cond
           ((not (pair? sub-tree)) (proc sub-tree))
           (else (tree-map proc sub-tree))))
       tree))
;
; Define square-tree using tree-map.
;
(define (square-tree tree)
  (tree-map (lambda (x) (* x x)) tree))

; Exercise 2.32.
;
; Define subsets.
;   Returns a list of all subsets of a set.
;
(define (subsets s)
  (if (null? s)
      '(())
      (let ((rest (subsets (cdr s))))
        (append rest 
                (map (lambda (x) (cons (car s) x)) rest)))))
;
(subsets '(1 2 3)) ; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
;
; If s is null, the procedure returns a list with the empty set.
; If s is not null, then rest has a list of the subsets of (cdr s).
; Now, map takes a procedure which cons'es the (car s) to each element
; of rest. Finally, rest is appended to the list returned by map.
;
