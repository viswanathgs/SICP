; Solutions to exercises 2.56 to 2.58 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Symbolic Differentiation
;
; A program that performs symbolic differentiation of algebraic expressions.
; Handles expressions that are built up using only the operations of addition
; and multiplication.
;
; Basic rules:
;
; dc/dx = 0 for constant c or a variable other than x.
; dx/dx = 1
; d(u+v)/dx = du/dx + dv/dx
; d(uv)/dx = u(dv/dx) + v(du/dx)
;

; Define deriv
;
; Takes as arguments an algebraic expression and a variable and returns
; the derivative of the expression with respect to the variable.
;
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

; Representation for algebraic expressions
;
; Define variable?
;
(define (variable? x)
  (symbol? x))
;
; Define same-variable?
;
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;
; Define make-sum
;
(define (make-sum a1 a2)
  (list '+ a1 a2))
;
; Define make-product
;
(define (make-product m1 m2)
  (list '* m1 m2))
;
; Define sum?
;
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
;
; Define product?
;
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
;
; Define addend
;
(define (addend s)
  (cadr s))
;
; Define augend
;
(define (augend s)
  (caddr s))
;
; Define multiplier
;
(define (multiplier p)
  (cadr p))
;
; Define multiplicand
;
(define (multiplicand p)
  (caddr p))

; Few examples
;
(deriv '(+ x 3) 'x) ; (+ 1 0)
(deriv '(* x y) 'x) ; (+ (* x 0) (* y 1))
(deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* (* x y) (+ 1 0)) (* (+ x 3) (+ (* x 0) (* y 1))))
;
; To simplify the expressions, we can just modify the constructors and selectors, without
; changing deriv at all.

; Define make-sum
;   If both the summands are numbers, make-sum will add them and return their sum.
;   If one of the summands evaluates to 0, then make-sum will return the other summand.
;
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
;
; Define =number?
;   Checks if an expression is equal to a given number.
;
(define (=number? exp num)
  (and (number? exp) (= exp num)))
;
; Define make-product
;
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; Examples
;
(deriv '(+ x 3) 'x) ; 1
(deriv '(* x y) 'x) ; y
(deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* (+ x 3) y))

; Exercise 2.56.
;
; Define deriv
;   Extending deriv to the following differentiation rule:
;   d(u^n)/dx = n * u^(n-1) * (du/dx)
;
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp) (make-sum (exponent exp) (- 1)))
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))
;
; Define constructor and selectors for exponentiation.
;
; Define make-exponentiation
;
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))
;
; Define exponentiation?
;
(define (exponentiation? s)
  (and (pair? s) (eq? (car s) '**)))
;
; Define base
;
(define (base s)
  (cadr s))
;
; Define exponent
;
(define (exponent s)
  (caddr s))
;
(deriv '(** x 5) 'x) ; (* 5 (** x 4))
(deriv '(** x n) 'x) ; (* n (** x (+ n -1)))
(deriv '(+ (** x 5) (* x y)) 'x) ; (+ (* 5 (** x 4)) y)

; Exercise 2.57.
;
; Modifying constructors and selectors to handle sums and products of
; arbitrary numbers of terms.
;
; Define make-sum
;
(define (make-sum . a)
  (append '(+) a))
;
; Define sum?
;
(define (sum? s)
  (and (pair? s) (eq? (car s) '+)))
;
; Define addend
;
(define (addend s)
  (cadr s))
;
; Define augend
;
(define (augend s)
  (let ((aug-list (cddr s)))
    (if (= (length aug-list) 1)
        (car aug-list)
        (append '(+) aug-list))))
;
; Define make-product
;
(define (make-product . m)
  (append '(*) m))
;
; Define product?
;
(define (product? s)
  (and (pair? s) (eq? (car s) '*)))
;
; Define multiplier
;
(define (multiplier s)
  (cadr s))
;
; Define multiplicand
;
(define (multiplicand s)
  (let ((mul-list (cddr s)))
    (if (= (length mul-list) 1)
        (car mul-list)
        (append '(*) mul-list))))
;
(deriv '(* x y (+ x 3)) 'x) ; (+ (* x (+ (* y (+ 1 0)) (* (+ x 3) 0))) (* (* y (+ x 3)) 1))

; Exercise 2.58.
;
; Modifying predicates, constructors and selectors to work with infix expressions.
;
; a. Assuming + and * always take two arguments and that expressions are fully paranthesized.
;
(define (make-sum a1 a2) (list a1 '+ a2))
;
(define (sum? s)
  (and (pair? s) (eq? (cadr s) '+)))
;
(define (addend s) (car s))
;
(define (augend s) (caddr s))
;
(define (make-product m1 m2) (list m1 '* m2))
;
(define (product? s)
  (and (pair? s) (eq? (cadr s) '*)))
;
(define (multiplier s) (car s))
;
(define (multiplicand s) (caddr s))
;
(deriv '(x + (3 * (x + (y + 2)))) 'x) ; (1 + ((3 * (1 + (0 + 0))) + ((x + (y + 2)) * 0)))
;
; b. With standard algebraic notation, which drops unnecessary parantheses, and
;    assuming multiplication is done before addition.
;
(define (make-sum . a)
  (letrec
      ((S (lambda (sum-list terms)
            (if (null? terms)
                sum-list
                (S (append sum-list (list '+ (car terms)))
                   (cdr terms))))))
    (cond ((= (length a) 1) (car a))
          (else (S (list (car a)) (cdr a))))))
;
(define (sum? s)
  (cond ((not (pair? s)) #f)
        ((memq '+ s) #t)
        (else #f)))
;
(define (addend s)
  (letrec
      ((A (lambda (lst)
            (if (eq? (car lst) '+)
                '()
                (cons (car lst) (A (cdr lst)))))))
    (let ((add-exp (A s)))
      (if (= (length add-exp) 1)
          (car add-exp)
          add-exp))))
;
(define (augend s)
  (let ((aug (cdr (memq '+ s))))
    (if (= (length aug) 1)
        (car aug)
        aug)))
;
(define (make-product . a)
  (letrec
      ((M (lambda (mul-list terms)
            (if (null? terms)
                mul-list
                (M (append mul-list (list '* (car terms)))
                   (cdr terms))))))
    (cond ((= (length a) 1) (car a))
          (else (M (list (car a)) (cdr a))))))
;
(define (product? s)
  (cond ((not (pair? s)) #f)
        ((memq '+ s) #f)
        ((memq '* s) #t)
        (else #f)))
;
(define (multiplier s)
  (letrec
      ((M (lambda (lst)
            (if (eq? (car lst) '*)
                '()
                (cons (car lst) (M (cdr lst)))))))
    (let ((mul-exp (M s)))
      (if (= (length mul-exp) 1)
          (car mul-exp)
          mul-exp))))
;
(define (multiplicand s)
  (let ((mul (cdr (memq '* s))))
    (if (= (length mul) 1)
        (car mul)
        mul)))
;
(deriv '(x + 3 * (x + y + 2)) 'x) ; (1 + ((3 * (1 + (0 + 0))) + ((x + y + 2) * 0)))