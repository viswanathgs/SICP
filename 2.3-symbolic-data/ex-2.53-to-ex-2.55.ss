; Solutions to exercises 2.53 to 2.55 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Define memq
;   Parameters: item - a symbol
;               x - a list
;
(define (memq item x)
  (cond
   ((null? x) #f)
   ((eq? item (car x)) x)
   (else (memq item (cdr x)))))
;
(memq 'apple '(pear banana prune)) ; #f
(memq 'apple '(x (apple sauce) y apple pear)) ; (apple pear)

; Exercise 2.53.
;
(list 'a 'b 'c) ; (a b c)
(list (list 'george)) ; ((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks)) ; (red shoes blue socks)

; Exercise 2.54.
;
; Define equal?
;   Equality check for lists.
;
(define (equal?- ls1 ls2)
  (cond
   ((null? ls1) (null? ls2))
   ((and (not (pair? ls1)) (not (pair? ls2))) (eq? ls1 ls2))
   ((or (not (pair? ls1)) (not (pair? ls2))) #f)
   (else (and (equal? (car ls1) (car ls2))
              (equal? (cdr ls1) (cdr ls2))))))
;
(equal?- '(this is a list) '(this is a list)) ; #t
(equal?- '(this is a list) '(this (is a) list)) ; #f

; Exercise 2.55.
;
(car ''abracadabra) ; quote
;
; The symbol ' is just a shorthand for (quote ...).
; Hence, 'abracadabra actually is (quote abracadabra).
; Now, (car ''abracadabra)
;      = (car '(quote abracadabra))
;      = quote
;