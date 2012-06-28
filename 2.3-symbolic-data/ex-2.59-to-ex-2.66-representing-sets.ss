; Solutions to exercises 2.59 to 2.66 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Representing Sets
;

; Sets as unordered lists
;
; Define element-of-set?
;   Returns #t if x is found in set, else #f.
;
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
;
; Define adjoin-set
;   Adds x to set.
;
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
;
; Define intersection-set
;   Returns the intersection of set1 and set2.
;
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; Exercise 2.59.
;
; Define union-set
;   Returns the union of set1 and set2.
;
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

; Exercise 2.60.
;
; Modifying the representation to allow duplicate elements (multiset).
;
; Define element-of-set?
;   No change in this procedure.
;
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
;
; Define adjoin-set
;   Since we allow duplicates, x can be added to set without using element-of-set?.
;   Constant complexity.
;
(define (adjoin-set x set)
  (cons x set))
;
; Define union-set
;   The two sets can be directly combined without using element-of-set?.
;   Complexity: O(n)
;
(define (union-set set1 set2)
  (append set1 set2))
;
; Define intersection-set
;   No change in this procedure.
;
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; Sets as ordered lists
;   (Assuming all the elements are numbers)
;
; Define element-of-set?
;   Terminate search when (car set) > x.
;   n/2 steps on average.
;
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))
;
; Define intersection-set
;   Complexity: O(n)
;
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((= (car set1) (car set2))
         (cons (car set1) (intersection-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (intersection-set (cdr set1) set2))
        (else
         (intersection-set set1 (cdr set2)))))

; Exercise 2.61.
;
; Define adjoin-set
;   n/2 steps on average.
;
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

; Exercise 2.62.
;
; Define union-set
;   Complexity: O(n)
;
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                 (else (cons x2 (union-set set1 (cdr set2)))))))))

; Sets as binary trees
;
; Each node in the tree is represented as a list of three items - entry at the node,
; left subtree and right subtree.
;
; Define make-tree
;
(define (make-tree entry left right)
  (list entry left right))
;
; Define entry
;
(define (entry tree)
  (car tree))
;
; Define left-branch
;
(define (left-branch tree)
  (cadr tree))
;
; Define right-branch
;
(define (right-branch tree)
  (caddr tree))

; Define element-of-set?
;
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        (else
         (element-of-set? x (right-branch set)))))
;
; Define adjoin-set
;
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        (else
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; Exercise 2.63.
;
; Procedures that convert a binary tree to a list.
;
; Define tree->list-1
;
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
;
; Define tree->list-2
;
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))
;
(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
(tree->list-1 tree1)
(tree->list-2 tree1)
(tree->list-1 tree2)
(tree->list-2 tree2)
(tree->list-1 tree3)
(tree->list-2 tree3)
;
; a. Both the procedures produce the same result for every tree (the inorder traversal).
; b. Both the procedures run in O(n).
;

; Exercise 2.64.
;
; Define list->tree
;   Converts an ordered list to a balanced binary tree.
;
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
;
; Define partial-tree
;   Takes as arguments an integer n and a list of at least n elements
;   and constructs a balanced tree containing the first n elements of
;   the list.
;   Returns a pair whose car is the constructed tree and cdr is the list
;   of elements not included in the tree.
;
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
;
; The sizes of the left and right subtrees are calculated as floor((n-1)/2)
; and n - left-size - 1. Calling partial-tree on the same list but with left-size
; produces returns the left subtree and the remaining elements. Of the remaining
; elements, the car is the entry of the current node, and partial-tree is called
; on the cdr of the remaining elements with right-size. This produces the right
; subtree and the elements that are not included in the tree at all.
;
(list->tree '(1 3 5 7 9 11)) ; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;
;       5
;      / \
;     /   \
;    1     9
;     \   / \
;      3 7  11
;
; Time complexity: O(n)
;

; Exercise 2.65
;
; Define union-set (for balanced binary trees)
;
(define (union-set-binary-tree set1 set2)
  (let ((list1 (tree->list-1 set1))
        (list2 (tree->list-1 set2)))
    (list->tree (union-set list1 list2))))
;
; Define intersection-set (for balanced binary trees)
;
(define (intersection-set-binary-tree set1 set2)
  (let ((list1 (tree->list-1 set1))
        (list2 (tree->list-1 set2)))
    (list->tree (intersection-set list1 list2))))

; Sets and information retrieval
;
; Define lookup (for a set of records represented as an unordered list)
;
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))
;
; Exercise 2.66.
;
; Define lookup (for a set of records represented as a binary tree)
;
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((cur-key (key (entry set-of-records))))
        (cond ((= given-key cur-key) (entry set-of-records))
              ((< given-key cur-key) (lookup given-key (left-branch set-of-records)))
              (else (lookup given-key (right-branch set-of-records)))))))