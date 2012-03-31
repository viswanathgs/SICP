; Solutions to exercises 1.1 to 1.5 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at 
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;


; Exercise 1.1. 
;
; Value printed by the interpreter for each expression.
;
10                               ; 10
(+ 5 3 4)                        ; 12
(- 9 1)                          ; 8
(+ (* 2 4) (- 4 6))              ; 6
(define a 3)                     ; a = 3
(define b (+ a 1))               ; b = 4
(= a b)                          ; #f
(if (and (> b a) (< b (* a b)))
  b
  a)                             ; 4
(cond
  ((= a 4) 6)
  ((= b 4) (+ 6 7 a))
  (else 25))                     ; 16
(+ 2 (if (> b a) b a))           ; 6
(* (cond
     ((> a b) a)
     ((< a b) b)
     (else -1))
   (+ a 1))                      ; 16

; Exercise 1.2. 
;
; Translating an expression into prefix form.
;
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) 
   (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3. 
;
; Define a procedure that takes three numbers as arguments 
; and returns the sum of the squares of the two larger numbers.
;
(define (sum-of-squares-large a b c)
  (cond
    ((and (<= a b) (<= a c)) (+ (* b b) (* c c)))
    ((and (<= b a) (<= b c)) (+ (* a a) (* c c)))
    (else (+ (* a a) (* b b)))))

; Exercise 1.4. 
;
; Observe that our model of evaluation allows for 
; combinations whose operators are compound expressions. Use this 
; observation to describe the behavior of the following procedure:
;
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;
; The if statement returns a primitive procedure (an operator), + or -,
; depending on whether b > 0 or not. The returned procedure takes 
; two arguements a and b.


; Exercise 1.5. 
;
; Ben Bitdiddle has invented a test to determine whether 
; the interpreter he is faced with is using applicative-order evaluation 
; or normal-order evaluation. He defines the following two procedures:
;
(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))
;
; Then he evaluates the expression
;
(test 0 (p))
;
; What behavior will Ben observe with an interpreter that uses applicative-order 
; evaluation? What behavior will he observe with an interpreter that uses 
; normal-order evaluation? Explain your answer. (Assume that the evaluation rule 
; for the special form if is the same whether the interpreter is using normal 
; or applicative order: The predicate expression is evaluated first, and the 
; result determines whether to evaluate the consequent or the alternative expression.)
;
; With applicative-order evaluation, the arguments are first evaluated before applying
; them to the procedure. Here, the second argument to test is (p), whose definition is 
; again (p). When this is evaluated recursively, we end up in an infinite loop. The 
; program never terminates with applicative-order evaluation.
;
; With normal-order evaluation, the evaluation of arguments is done only when required
; (lazy evaluation). The argument (p) is passed to the test procedure without being
; evaulated. Inside test, since the if clause is true, the procedure just returns 0.
;

