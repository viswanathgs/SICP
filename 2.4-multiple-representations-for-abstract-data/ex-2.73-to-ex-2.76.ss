; Solutions to exercises 2.53 to 2.55 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-17.html#%_sec_2.4
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Representations for Complex Numbers
;
; Abtraction for complex numbers that are represented in both forms: rectangular
; form (real and imaginary parts) and polar form (magnitude and angle).
;

; Basic arithmetic on complex numbers (abstracted above the representation layer)
;
; Define add-complex
;   This uses the rectangular form. Just add the corresponding parts together.
;
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
;
; Define sub-complex
;
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
;
; Define mul-complex
;   This uses the polar form.
;   Polar form: x + iy = r(cos(t) + isin(t)) = r.e^(it)
;   Product: r.e^(ip) + s.e^(iq) = rs.e^(i(p+q))
;
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
;
; Define div-complex
;
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

; Constructors and selectors
;
; Rectangular form
;   Complex numbers are represented as pairs of real and imaginary parts.
;
; Define real-part
;
(define (real-part z) (car z))
;
; Define imag-part
;
(define (imag-part z) (cdr z))
;
; Define magnitude
;   magnitude = sqrt (x^2 + y^2)
;
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
;
; Define angle
;   angle = atan(y/x)
;
(define (angle z)
  (atan (imag-part z) (real-part z)))
;
; Define make-from-real-imag
;
(define (make-from-real-imag x y) (cons x y))
;
; Define make-from-mag-ang
;   x = r * cos(a), y = r * sin(a)
;
(define (make-from-mag-ang r a)
  (make-from-real-imag (* r (cos a)) (* r (sin a))))
;
; Polar form
;   Complex numbers are represented as pairs of magnitude and angle.
;
; Define real-part
;   x = r * cos(a)
;
(define (real-part z)
  (* (magnitude z) (cos (angle z))))
;
; Define imag-part
;   y = r * sin(a)
;
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
;
; Define magnitude
;
(define (magnitude z) (car z))
;
; Define angle
;
(define (angle z) (cdr z))
;
; Define make-from-real-imag
;
(define (make-from-real-imag x y)
  (make-from-mag-ang (sqrt (+ (square x) (square y)))
                     (atan y x)))
;
; Define make-from-mag-ang
;
(define (make-from-mag-ang r a) (cons r a))

; Tagged data (Dispatching on Type)
;   In the previous case, the system of arithmetic operations can only use either the constructors
;   and selectors of the rectangular form or those of the polar form. Both the representations cannot
;   be used in the same system (since, otherwise, a complex number represented as a pair (a,b) would
;   be ambiguous as to which form it is represented in).
;
;   To use both the representations in the same system without ambiguity, we include a type tag -- the
;   symbol 'rectangular or 'polar as part of each complex number. We need to include an abstraction layer
;   between the arithmetic layer and the multiple-representation layer to select the appropriate constructors
;   and selectors for the representation.
;

; Constructors, selectors and predicate for the type tag
;
; Define attach-tag
;
(define (attach-tag type-tag contents)
  (cons type-tag contents))
;
; Define type-tag
;
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
;
; Define contents
;
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
;
; Define rectangular?
;
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
;
; Define polar?
;
(define (polar? z)
  (eq? (type-tag z) 'polar))

; The constructors and selectors for the rectangular and polar forms are modified to include
; type tags and renamed so that their names do not conflict. This would enable both the
; representations to coexist in the same system.
;
; Constructors and selectors for rectangular form
;
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))
;
; Constructors and selectors for polar form
;
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar) (car z))
(define (angle-polar) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

; Generic selectors and constructors
;   These work on both the representations. Each procedure checks the tag of its argument and calls
;   the appropriate procedure for handling data of that type (representation).
;
(define (real-part z)
  (cond ((rectangular? z) (real-part-rectangular (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z) (magnitude-rectangular (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))
;
; The constructors can construct complex numbers in either representation.
; For simplicity, we choose the same representation as the arguments to the constructor.
;
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

; Data-Directed Programming and Additivity
;   (Refer section 2.4.3 for data-directed programming)
;
;   Data-directed programming is just like polymorphism. Here, we use an explicit table indexed by the generic procedure
;   and the data type (representation) to call the appropriate procedure for the type.
;
;   Unlike tagged data representation, we do not have to modify the generic procedures to add new data representations.
;   Instead, we just add new entries to the table.
;

; Procedures for manipulating the operation-and-type table
;   (Assume put and get are implemented for now. They are described in chapter 3)
;
; (put <op> <type> <item>) installs the <item> in the table, indexed by <op> and <type>
; (get <op> <type>) looks up the <op>, <type> entry in the table and returns the item found there. Returns #f if no item is found.
;

; Procedures belonging to each representation is encapsulated within a package. These procedures interface
; with the rest of teh system by adding entries to the table.
;
; Package for rectangular form
;
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
;
; The internal procedures of different packages would not result in naming conflicts even if they have
; the same names, due to difference in scope. Also, the internal procedures do not have to deal with
; tagging the data. They are implemented as if there is no other representation for the data. The interface
; section deals with tagging and adding entries to the table.
;
; Note:
;   In the interface section, for selectors, we use the list '(rectangular) rather than the symbol
;   'rectangular to allow for the possibility of operations with multiple arguments, not all of
;   the same type.  The type the constructors are installed under needn't be a list because a
;   constructor is always used to make an object of one particular type.
;
; Package for polar form
;
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; Define apply-generic
;   Takes as arguments a generic operation and a variable length list of arguments. It looks in
;   the table for an entry indexed by the name of the operation and the types of the arguments
;   and applies the resulting procedure of the arguments.
;
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))
;
; Generic selectors using apply-generic
;   Since they do not directly involve with the type tags, adding new representations to the table
;   does not result in any change in generic selectors.
;
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
;
; Constructors
;   Note that we cannot use apply-generic here. Arguments to apply-generic are always a list of
;   complex numbers, and are used for procedures with arguments as complex numbers.
;
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))