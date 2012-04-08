; Solutions to exercises 2.44 to 2.52 of The Wizard Book: Structure and 
; Interpretation of Computer Programs (SICP), by Hal Abelson, Jerry Sussman 
; and Julie Sussman.
;
; The corresponding section of the book can be found at
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; A Picture Language
; (Refer the book for images)
;
; Painter - element that draws an image that is shifted and scaled to fit
; within a designated parallelogram shaped frame.
; 
; wave is a primitive painter that makes a crude line drawing.
;
; beside is an operation that takes two painters and produces a compound painter (hence
; satisfies closure) that draws the first painter's image in the left half of the frame 
; and the second painter's image in the right half.
;
; below is an operation that takes two painters and produces a compound painter that draws
; the first painter's image below the second painter's image.
;
; flip-vert produces a painter that draws the image upside-down.
;
; flip-horiz produces a painter that draws the image left-to-right reversed.
;
; Define wave2 and wave4
;
(define wave2 (beside (wave (flip-vert wave))))
(define wave4 (below wave2 wave2))
;
; Abstraction for the pattern in wave4
;
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
;
; Define wave4 in terms of flipped-pairs
;
(define wave4 (flipped-pairs wave))
;
; Define right-split
;   Makes painters split and branch towards the right.
;
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
;
; Define corner-split
;   Balanced pattern that branches upwards as well as towards the right.
;
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;
; Define square-limit
;   Symmetric combination of four corner-split figures.
;
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
;
; Exercise 2.44.
;
; Define up-split.
;  Makes painters split and branch towards the top.
;
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; Higher-order operations
;   Abstraction for painter operations.
;
; Define square-of-four
;   Takes four one-argument painter operations (top-left, top-right
;   bottom-left and bottom-right), and produces a painter operation
;   that transforms a given painter with those four operations and 
;   arranges the result in a square.
;
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
;
; Define flipped-pairs in terms of square-of-four
;
(define (flipped-pairs painter)
  ((square-of-four identity flip-vert 
                   identity flip-vert) painter))
;
; Define square-limit in terms of square-of-four
;
(define (square-limit painter)
  ((square-of-four flip-horiz identity
                   rotate180 flip-vert) painter))

; Exercise 2.45.
;
; Define split
;   Abstracting split operation.
;
(define (split main-op sub-op)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split main-op sub-op) painter (- n 1))))
          (main-op painter (sub-op smaller smaller))))))
;
; Define right-split in terms of split
;
(define right-split (split beside below))
;
; Define up-split in terms of split
;
(define up-split (split below beside))

; Frames
;
; A frame is described by three vectors - an origin vector and two edge vectors.
;
; Define frame-coord-map
;   A frame coordinate map is associated to each frame, and is used to shift and
;   scale images to fit the frame. The map transforms the unit square (that contains
;   the image) into the frame by mapping the vector v = (x, y) to the vector sum
;   origin(frame) + x . edge1(frame) + y . edge2(frame).
;
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                (scale-vect (ycor-vect v) (edge2-frame frame))))))
;
; Define origin-frame in terms of frame-coord-map
;
(define (origin-frame a-frame)
  ((frame-coord-map a-frame) (make-vect 0 0)))

; Exercise 2.46.
;
; Data abstraction for vectors (constructor and selectors).
;
(define (make-vect x y)
  (list x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cadr vect))
;
; Define add-vect
;
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
;
; Define sub-vect
;
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
;
; Define scale-vect
;
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; Exercise 2.47.
;
; Constructor and selectors for frames
;
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))
;
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (cddr frame))

; Painters
;   A painter is a procedure that takes a frame as an argument
;   and draws a particular image shifted and scaled to fit the 
;   frame.
;
; Define segments->painter
;   This procedure takes a list of line segments as an argument, 
;   transforms the segment endpoints with the frame coordinate map
;   and draws a line between the transformed points for each segment.
;
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment)))) 
      segment-list)))

; Exercise 2.48.
;
; Implementation of a directed line segment as a pair of vectors - the
; vector from the origin to the start-point of the segment, and the vector
; from the origin to the end-point.
;
(define (make-segment vect1 vect2)
  (cons vect1 vect2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

; Exercise 2.49.
;
; Define outline-painter
;   Painter that draws the outline of a frame.
;
(define outline-segments
  (list 
    (make-segment (make-vect 0 0) (make-vect 0 1))
    (make-segment (make-vect 0 0) (make-vect 1 0))
    (make-segment (make-vect 0 1) (make-vect 1 1))
    (make-segment (make-vect 1 0) (make-vect 1 1))))
(define (outline-painter frame)
  ((segments->painter outline-segments) frame))
;
; Define diagonal-painter
;   Painter that draws the diagonals of a frame.
;
(define diagonal-segments
  (list
    (make-segment (make-vect 0 0) (make-vect 1 1))
    (make-segment (make-vect 0 1) (make-vect 1 0))))
(define (diagonal-painter frame)
  ((segments->painter diagonal-segments) frame))
;
; Define diamond-painter
;   Painter that draws a diamond by connecting the midpoints
;   of the sides of a frame.
;
(define diamond-segments
  (list
    (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
    (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
    (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
    (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))
(define (diamond-painter frame)
  ((segments->painter diamond-segments) frame))
;
; wave painter is similar to the above, only with a different segment-list.
; 
