(declare (usual-integrations))
(load '("lib/assert" "lib/math"))

; {{{1 2.1.1 Example: Arithmetic Operations for Rational Numbers
; {{{2 Exercise 2.1:
; {{{3 Problem
;      Define a better version of make-rat that handles both positive and
;      negative arguments.  make-rat should normalize the sign so that if the
;      rational number is positive, both the numerator and denominator are
;      positive, and if the rational number is negative, only the numerator is
;      negative.
;
; {{{3 Solution
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (abs (gcd n d)))
        (nn (if (or
                  (and (< n 0) (< d 0))
                  (and (> n 0) (< d 0)))
              (* n -1)
              n))
        (nd (if (or
                  (and (< n 0) (< d 0))
                  (and (> n 0) (< d 0)))
              (* d -1)
              d)))
    (cons (/ nn g) (/ nd g))))

(assert (let ((rat (make-rat 2 -4)))
          (and (= -1 (numer rat))
               (= 2 (denom rat))))
        "The procedure #make-rat works as expected when passed (2 -4)")

(assert (let ((rat (make-rat -2 4)))
          (and (= -1 (numer rat))
               (= 2 (denom rat))))
        "The procedure #make-rat works as expected when passed (-2 4)")

(assert (let ((rat (make-rat -2 -4)))
          (and (= 1 (numer rat))
               (= 2 (denom rat))))
        "The procedure #make-rat works as expected when passed (-2 -4)")

(assert (let ((rat (make-rat 2 4)))
          (and (= 1 (numer rat))
               (= 2 (denom rat))))
        "The procedure #make-rat works as expected when passed (2 4)")

(assert (let ((rat (make-rat 4 -2)))
          (and (= -2 (numer rat))
               (= 1 (denom rat))))
        "The procedure #make-rat works as expected when passed (4 -2)")

; {{{1 2.1.2 Abstraction Barriers
; {{{2 Exercise 2.2:
; {{{3 Problem
;      Consider the problem of representing line segments
;      in a plane.  Each segment is represented as a pair of points: a
;      starting point and an ending point.  Define a constructor
;      #make-segment and selectors #start-segment and #end-segment
;      that define the representation of segments in terms of points.
;      Furthermore, a point can be represented as a pair of numbers: the
;      x coordinate and the y coordinate.  Accordingly, specify a
;      constructor #make-point and selectors #x-point and #y-point
;      that define this representation.  Finally, using your selectors
;      and constructors, define a procedure #midpoint-segment that takes
;      a line segment as argument and returns its midpoint (the point
;      whose coordinates are the average of the coordinates of the
;      endpoints).  To try your procedures, you'll need a way to print
;      points:

; {{{3 Solution
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(assert (equal? (cons 1 2) (make-point 1 2))
        "Procedure #make-point works as expected when passed 1, 2.")
(assert (= 1  (x-point (make-point 1 2)))
        "Procedure #x-point works as expected.")
(assert (= 2  (y-point (make-point 1 2)))
        "Procedure #y-point works as expected.")
(assert (equal? (cons 0 0)
                (start-segment (make-segment (make-point 0 0) (make-point 2 2))))
        "Procedure #start-segment and #make-segment work as expected.")
(assert (equal? (cons 2 2)
                (end-segment (make-segment (make-point 0 0) (make-point 2 2))))
        "Procedure #end-segment works as expected.")
(assert (equal? (cons 1 1) (midpoint-segment (make-segment (make-point 0 0) (make-point 2 2))))
        "Procedure #midpoint-segment works as expected.")
(assert (equal? (cons 3/2 3/2) (midpoint-segment (make-segment (make-point 0 0) (make-point 3 3))))
        "Procedure #midpoint-segment works as expected.")

; {{{2 Exercise 2.3:
; {{{3 Problem
;      Implement a representation for rectangles in a plane.  (Hint: You
;      may want to make use of Exercise 2-2.)  In terms of your
;      constructors and selectors, create procedures that compute the
;      perimeter and the area of a given rectangle.  Now implement a
;      different representation for rectangles.  Can you design your
;      system with suitable abstraction barriers, so that the same
;      perimeter and area procedures will work using either
;      representation?
;
; {{{3 Solution
