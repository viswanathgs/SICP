; Solutions to exercises 2.7 to 2.16 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at 
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-14.html#%_sec_2.1
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Extended Exercise: Interval Arithmetic
;
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y)) 
                               (/ 1.0 (lower-bound y)))))

; Exercise 2.7.
;
; Implementation of the interval abstraction (constructor and selectors).
;
(define (make-interval a b)
  (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

; Exercise 2.8.
; 
; Difference of two intervals.
;
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Exercise 2.9.
;
; Width of intervals.
;
(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2.0))
;
; Width of the sum or difference of two intervals = sum of 
; the widths of the individual intervals. 
;
(define (width-sum int-x int-y)
  (+ (width int-x) (width int-y)))
;
; Such a formulation for width cannot be obtained for multiplication
; or divison of intervals.
;

; Exercise 2.10.
;
(define (div-interval x y)
  (cond
    ((spans-zero? y) (error "y spans zero."))
    (else (mul-interval x
                        (make-interval (/ 1.0 (upper-bound y))
                                       (/ 1.0 (lower-bound y)))))))
(define (spans-zero? int)
  (cond
    ((or (< (upper-bound int) 0.0) (> (lower-bound int) 0.0)) #f)
    (else #t)))

; Exercise 2.11.
;
; Rewrite mul-interval by breaking into nine cases.
;
(define (pos? int) (> (lower-bound int) 0.0))
(define (neg? int) (< (upper-bound int) 0.0))
(define (mul-interval x y)
  (let ((lbx (lower-bound x))
        (ubx (upper-bound x))
        (lby (lower-bound y))
        (uby (upper-bound y)))
    (cond
      ((pos? x)
        (cond
          ((pos? y) (make-interval (* lbx lby) (* ubx uby)))
          ((neg? y) (make-interval (* ubx lby) (* lbx uby)))
          (else (make-interval (* ubx lby) (* ubx uby)))))
      ((neg? x)
        (cond
          ((pos? y) (make-interval (* lbx uby) (* ubx lby)))
          ((neg? y) (make-interval (* ubx uby) (* lbx lby)))
          (else (make-interval (* lbx uby) (* lbx lby)))))
      (else
        (cond
          ((pos? y) (make-interval (* lbx uby) (* ubx uby)))
          ((neg? y) (make-interval (* ubx lby) (* lbx lby)))
          (else
            (make-interval (min (* lbx uby) (* ubx lby))
                           (max (* lbx lby) (* ubx uby)))))))))

; Exercise 2.12.
;
; Interval representation using center and tolerance.
;
(define (make-center-percent center percent)
  (let ((width (abs (* (/ percent 100.0) center))))
    (make-interval (- center width) (+ center width))))
(define (center int)
  (/ (+ (lower-bound int) (upper-bound int)) 2.0))
(define (width int)
  (/ (- (upper-bound int) (lower-bound int)) 2.0))
(define (percent int)
  (* (/ (width int) (abs (center int))) 100.0))

; Exercise 2.13.
;
; Approximate percentage tolerance of the product of two
; positive intervals.
;
; z = x * y
; (x + dx) * (y + dy) = xy + x * dy + y * dx + dx * dy
; Approximately, dz = x * dy + y * dx
; Percentage tolerance = dz/z * 100 = (x * dy + y * dx)/(x * y) * 100
; Percentage tolerance = (dy/y + dx/x) * 100
;

; Exercise 2.14.
;
; Two equivalent ways for calculating the combined resistance of resistors
; in parallel.
;
; Formula 1 : (r1 * r2) / (r1 + r2)
;
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
;
; Formula 2: 1 / ((1/r1) + (1/r2))
;
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
;
(define r1 (make-interval 100 100.5))
(define r2 (make-interval 200 200.5))
(par1 r1 r2) ; (66.44518272425249 . 67.1675)
(par2 r1 r2) ; (66.66666666666667 . 66.94435215946844)
;
(define r1 (make-interval 100 100))
(define r2 (make-interval 200 200))
(par1 r1 r2) ; (66.66666666666667 . 66.66666666666667)
(par2 r1 r2) ; (66.66666666666667 . 66.66666666666667)
;
(define r1 (make-center-percent 100 5))
(define r2 (make-center-percent 200 5))
(center (par1 r1 r2)) ; 67.33500417710944
(width (par1 r1 r2)) ; 10.033416875522136
(center (par2 r1 r2)) ; 66.66666666666666
(width (par2 r1 r2)) ; 3.3333333333333357
;
(define r1 (make-center-percent 100 1))   
(define r2 (make-center-percent 200 1))
(center (par1 r1 r2)) ; 66.6933360002667
(width (par1 r1 r2)) ; 2.0002666933359947
(center (par2 r1 r2)) ; 66.66666666666667
(width (par2 r1 r2)) ; .6666666666666714
;
; When the width of the interval or the percentage tolerance increases,
; the difference in the values returned by par1 and par2 increases.
;
; This is because, due to the implementation of div-interval, par1 
; and par2 apply different formulas for calculation.
;
; Let lb1 and ub1 be the lower and upper bounds respectively of r1.
; Assuming all the values are positive,
; lower bound of par1 = (lb1 * lb2) / (ub1 + ub2)
; lower bound of par2 = (lb1 * lb2) / (lb1 + lb2)
;
; Also, (r1/r1) results in the interval (lb1/ub1, ub1/lb1). This is not
; equal to the interval (1, 1) when lb1 != ub1.
;
; Similarly, the formulae for upper bounds of par1 and par2 can be
; obtained. The difference in formulae leads to the difference in the 
; returned values. When the width or tolerance is less, there is little
; difference between the lower and upper bounds, and hence there is little
; difference between the values returned by par1 and par2.
;

; Exercise 2.15.
;
; From the example results above, par2 seems to have tighter error bounds.
; This is a direct result of the difference in formula in par1 and par2 for
; lower and upper bounds.
;

; Exercise 2.16.
;
; Intervals do not have identity elements or inverses. They do not follow
; properties like the law of distribution. The way to solve this problem 
; is to identify if two operations are equal and apply the values accordingly.
; (For a better explanation, refer Eli Bendersky's site).
