; Solutions to exercises 2.67 to 2.72 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Huffman Encoding Trees
;   

; Representing Huffman trees
;
; Constructor for leaves
;
; Define make-leaf
;   Leaves are represented by a list containing the symbol 'leaf, the symbol at the leaf
;   and the weight.
;
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
;
; Predicate for leaves
;
; Define leaf?
;
(define (leaf? object)
  (eq? (car object) 'leaf))
;
; Selectors for leaves
;
; Define symbol-leaf
;
(define (symbol-leaf x)
  (cadr x))
;
; Define weight-leaf
;
(define (weight-leaf x)
  (caddr x))
;
; Constructor for non-leaf nodes
;
; Define make-code-tree
;   A general tree is represented as a list of a left branch, a right branch, a set of
;   symbols (for the sake of simplicity, the set is represented as an unordered list), and
;   a weight.
;   The procedure takes the left and right subtrees as arguments, and produces a tree by merging
;   them. The set of symbols of the new node is the union of the sets in the sub-nodes, and the
;   new weight is the sum of the weights of the sub-nodes.
;   
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
;
; Selectors for non-leaf nodes
;
; Define left-branch
;
(define (left-branch tree) (car tree))
;
; Define right-branch
;
(define (right-branch tree) (cadr tree))
;
; Generic selectors
;
; Define symbols
;
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
;
; Define weight
;
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; The decoding procedure
;
; Define decode
;   Takes as arguments a list of bits and a Huffman tree. Returns a list of decoded symbols.
;
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= 0 bit) (left-branch branch))
        ((= 1 bit) (right-branch branch))
        (else (error "bad bit -- CHOOSE BRANCH" bit))))

; Sets as weighted elements
;
; Tree-generating algorithm requires sets of leaves and trees (successively merging the
; two smallest items). In this case, it is better to use an ordered list representation
; for the set (to find the smallest items).
;
; Define adjoin-set
;   A set of leaves and trees is represented as a list of elements, arranged in increasing
;   order of weight.
;
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))
;
; Define make-leaf-set
;   Takes a list of symbol-frequency pairs such as ((A 4) (B 2) (C 8)) and constructs
;   an initial ordered set of leaves, that can be used to generate the Huffman tree by
;   successive merging.
;
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; Exercise 2.67.
;
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
;
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;
(decode sample-message sample-tree) ; (a d a b b c a)

; Exercise 2.68.
;
; Define encode
;   Takes as arguments a list of symbols representing the message and a Huffman tree.
;   Returns a list of encoded bits.
;
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
;
; Define encode-symbol
;   Takes a symbol and a Huffman tree. Returns a list of encoded bits for that symbol.
;
(define (encode-symbol symbol tree)
  (if (not (element-of-set? symbol (symbols tree)))
      (error "invalid symbol -- ENCODE-SYMBOL" symbol)
      (letrec ((E (lambda (code cur-branch)
                    (cond ((leaf? cur-branch) code)
                          ((element-of-set? symbol (symbols (left-branch cur-branch)))
                           (E (append code (list '0)) (left-branch cur-branch)))
                          (else
                           (E (append code (list '1)) (right-branch cur-branch)))))))
        (E '() tree))))
;
; Assuming the Huffman tree is correct, an invalid symbol can be detected at the root node itself,
; since the set of symbols at the root should contain the symbols in all the leaves. Hence, at each
; non-leaf node, if the symbol is not found in the left branch, it has to be in the right branch.
;
; Define element-of-set?
;   encode-symbol uses this procedure at each node on the set of symbols (represented by unordered lists)
;   to decide the branch to take.
;
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
;
(encode '(a d a b b c a) sample-tree) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)
(encode '(e) sample-tree) ; invalid symbol -- ENCODE-SYMBOL e

; Exercise 2.69.
;
; Define generate-huffman-tree
;   Takes a list of symbol-frequency pairs (where no symbol appears in more than
;   one pair) and generates a Huffman encoding tree according to the Huffman algorithm.
;
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
;
; Define successive-merge
;   Takes a set of leaves and merges them successively until there is only one element
;   left, which is the desired Huffman tree.
;
(define (successive-merge combined-set)
  (if (= (length combined-set) 1)
      (car combined-set)
      (successive-merge (adjoin-set (make-code-tree (car combined-set) (cadr combined-set))
                                    (cddr combined-set)))))
;
(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))) ; ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)
;
(define given-tree (generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1))))
(encode '(C A B) given-tree) ; (1 1 0 1 0 1 1 1)
(decode '(1 1 0 1 0 1 1 1) given-tree) ; (c a b)

; Exercise 2.70.
;
; Symbol-frequency-pairs
;
(define 1950-rock-lyrics '((A 2) (NA 16)
                           (BOOM 1) (SHA 3)
                           (GET 2) (YIP 9)
                           (JOB 2) (WAH 1)))
;
; Generate Huffman tree
;
(define 1950-rock-huffman-tree (generate-huffman-tree 1950-rock-lyrics))
;
; Message to be encoded
;
(define 1950-rock-song '(Get a job
                         Sha na na na na na na na na
                         Get a job
                         Sha na na na na na na na na
                         Wah yip yip yip yip yip yip yip yip yip
                         Sha boom))
;
; Encode the message
;
(define encoded-rock-song (encode 1950-rock-song 1950-rock-huffman-tree))
(length encoded-rock-song) ; 84 (number of bits after encoding)
;
; Decode the bits
;
(define decoded-rock-song (decode encoded-rock-song 1950-rock-huffman-tree))
(equal? decoded-rock-song 1950-rock-song) ; #t (same as the original message)
;
; If fixed-length code was used for the 8-symbol alphabet, then each symbol requires 3 bits.
; The message has 36 symbols in total.
; Hence, (* 36 3) = 108 bits would be the length of the encoded message.
; Huffman encoding has resulted in 22.22% reduction in length of the encoded message (84 bits).
;

; Exercise 2.71.
;
; Number of symbols, n = 5.
; The relative frequencies of symbols are 1, 2, 4, 8, 16.
;
; Since 2^i > (2^1 + 2^2 + ... + 2^(i-1)), the Huffman tree will be as follows: 
; 
;      /\
;     A /\
;      B /\
;       C /\
;        D  E
;
; Similar tree will be generated for n = 10.
;
; The tree will be linear, at the end of the chain.
; Least frequent symbol requires 1 bit.
; Most frequent symbol requires (n-1) bits.
;

; Exercise 2.71.
;
; For n symbols, the maximum height of the tree can be O(n). Since the sets are represented as
; unordered lists, element-of-set? takes O(n). So, the total complexity to encode a symbol is
; O(n^2).
; If the sets are implemented as binary search trees, then the complexity reduces to O(nlogn).
;