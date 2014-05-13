(declare (usual-integrations))
(load '("lib/assert" "lib/math"))

; {{{1 2.1.1 Example: Arithmetic Operations for Rational Numbers (2.1)
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

; {{{1 2.1.2 Abstraction Barriers (2.2 - 2.3)
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
;      At it's most basic a rectangle may be represented by either two points
;      that represent vertices at opposite diagonal corners, or by any vertex
;      and two integers, one representing the height and the other the length.
;
;      In order to be able to use the same area and perimeter procedure for
;      either of these representations, we would have to define them in terms
;      of the height and length of the rectangle and provide selectors that
;      return height or length given a rectangle.
;
(define (area-rec rectangle)
  (* (height-rec rectangle)
     (width-rec rectangle)))

(define (perimeter-rec rectangle)
  (+ (* 2 (height-rec rectangle))
     (* 2 (width-rec rectangle))))

(define (make-rec vertex height width)
  ; This is iteresting because not only can a rectangle be represented in
  ; different ways but also the data can be arranged differenlty depending on
  ; the need. For example, In this case I have several choices, among them are
  ; where and how to arrange the vertex, height and width.  I could either
  ; arrange them so that the height and the width come first and thus optimize
  ; for use cases where I would need to get the height and width.  On the other
  ; hand, I could make the vertex point come first, thus optimize for use
  ; cases where needing to know the position of the rectangles on a grid is
  ; important.
  (let ((hw-point (cons height width)))
    (cons vertex hw-point)))

(define (height-rec rectangle)
  (car (cdr rectangle)))

(define (width-rec rectangle)
  (cdr (cdr rectangle)))

(assert (= 20 (area-rec (make-rec (make-point 0 0) 5 4)))
        "Procedure #area-rec works as expected.")

(assert (= 5 (height-rec (make-rec (make-point 0 0) 5 4)))
        "Procedure #height-rec works as expected.")

(assert (= 4 (width-rec (make-rec (make-point 0 0) 5 4)))
        "Procedure #width-rec works as expected.")

(define (make-rec vertex1 vertex2)
  (cons vertex1 vertex2))

(define (height-rec rectangle)
  (abs (- (x-point (car rectangle))
          (x-point (cdr rectangle)))))

(define (height-width rectangle)
  (abs (- (y-point (car rectangle))
          (y-point (cdr rectangle)))))

(assert (= 20 (area-rec (make-rec (make-point 0 0) (make-point 5 4))))
        "Procedure #area-rec works as expected.")

(assert (= 5 (height-rec (make-rec (make-point 0 0) (make-point 5 4))))
        "Procedure #height-rec works as expected.")

(assert (= 4 (width-rec (make-rec (make-point 0 0) (make-point 5 4))))
        "Procedure #width-rec works as expected.")

; {{{1 2.1.3 What Is Meant by Data? (2.4 - 2.6)
;
; Since we will be redefining #cons,  #cdr, and #car. I need a way to bring
; them back to normal.
(define cache-cons cons)
(define cache-car car)
(define cache-cdr cdr)

; {{{2 Exercise 2.4:
; {{{3 Problem
;      Here is an alternative procedural representation of pairs.  For this
;      representation, verify that `(car (cons x y))` yields `x` for any
;      objects `x` and `y`.

          (define (cons x y)
            ; Returns a λ function that takes one argument and applies it to x and y.
            (lambda (m) (m x y)))

          (define (car z)
            (z (lambda (p q) p)))

;      What is the corresponding definition of #cdr? (Hint: To verify that
;      this works, make use of the substitution model of section 1.1.5.)
;
; {{{3 Solution
;
; First we  verify that the above #cons and  #car procedure work as expected.
; (cons 1 2)
; (λ (m) 1 2)
; (car (λ (m) 1 2))
; ((λ (m) (m 1 2) (λ (p q) p))
; (λ (1 2) p)
; 1
(assert (= 1 (car (cons 1 2)))
        "Verity that procedural #cons and #car work as expected.")

          (define (cdr z)
            ; The corresponding cdr implementation would simply return q.
            (z (lambda (p q) q)))

; (λ (m) 1 2)
; (car (λ (m) 1 2))
; ((λ (m) (m 1 2) (λ (p q) p))
; (λ (1 2) q)
; 2
(assert (= 2 (cdr (cons 1 2)))
        "Verity that procedural #cdr works as expected.")

; {{{2 Exercise 2.5:
;
; {{{3 Problem
;      Show that we can represent pairs of non-negative integers using
;      only numbers and arithmetic operations if we represent the pair
;      `a` and `b` as the integer that is the product `2^a 3^b`.  Give
;      the corresponding definitions of the procedures #cons, #car,
;      and #cdr.
;
; {{{3 Solution
;
;      Notice that we can divide any number of type `2^a 3^b` recursively by
;      2 until the point where the  division will no longer result in an
;      integer.  Thus, this provides a way of retrieving `a` from the number
;      and a complementary  procedure can be applied to retrieve `b`
(define (cons a b)
  (* (pow 2 a)
     (pow 3 b)))

(define (car x )
  (if (not (= 0 (modulo x 2)))
    0
    (+ 1 (car (/ x 2)))))

(define (cdr x )
  (if (not (= 0 (modulo x 3)))
    0
    (+ 1 (cdr (/ x 3)))))

(assert (= 7 (car (cons 7  12)))
        "Verity that arithmetic #car and #cons  work as expected.")

(assert (= 12 (cdr (cons 7  12)))
        "Verity that arithmetic #cdr works as expected.")

; {{{2 Exercise 2.6:
;
; {{{3 Problem
;
;      In case representing pairs as procedures wasn't mind-boggling
;      enough, consider that, in a language that can manipulate
;      procedures, we can get by without numbers (at least insofar as
;      non-negative integers are concerned) by implementing 0 and the
;      operation of adding 1 as

          (define zero
            ; Define zero as a λ function that when passed a function `f`
            ; returns another λ function that takes any attribute and returns
            ; the same attribute back.  In effect, we are nullifying `f`.  (We
            ; can see why this may be an apt definition for zero in this
            ; system).
            (lambda (f) (lambda (x) x)))

          (define (add-1 n)
            ; Although a bit more difficult to visualize, #add-1 will create
            ; a function that essentially composes `f` unto the return value of
            ; the inner λ function returned by any number in this system.  For
            ; example applying #add-1 to `zero` results in:
            ;
            ; (λ (f) (λ (x) (f x)))
            ;
            ; Note that where `zero` returned `x`, `one-p` returns `(f x)`
            ; (The above result is verified in the solution section).
            (lambda (f) (lambda (x) (f ((n f) x)))))

;      This representation is known as "Church numerals", after its
;      inventor, Alonzo Church, the logician who invented the λ
;      calculus.
;
;      Define `one` and `two` directly (not in terms of `zero` and
;      #add-1).  (Hint: Use substitution to evaluate `(add-1 zero)`).
;      Give a direct definition of the addition procedure `+' (not in
;      terms of repeated application of `add-1').

; {{{3 Cleanup:
(define cons cache-cons)
(define car cache-car)
(define cdr cache-cdr)

; {{{3 Solution
;
; Using the substitution method, we can evaluate `(add-1 zero)`:
;
; (add-1 zero)
; ((λ (f) (λ (x) (f ((n f) x)))) (λ (f) (λ (x) x)))
; ((λ (f) (λ (x) (f (((λ (f) (λ (x) x)) f) x)))))
; ((λ (f) (λ (x) (f ((λ (x) x) x)))))
; (λ (f) (λ (x) (f x)))
;
; Thus,
(define one
  ; Comparing `one` to `zero` we see that the effect of `f` on the final
  ; function is to be applied once instead of zero times.
  (lambda (f) (lambda (x) (f x))))
;
; Next we derive `two` in this system by applying the substitution method to
; `(add-1 one)`:
;
; (add-1 one)
; ((λ (f) (λ (x) (f ((n f) x)))) (λ (f) (λ (x) (f x))))
; (λ (f) (λ (x) (f (((λ (f) (λ (x) (f x))) f) x))))
; (λ (f) (λ (x) (f ((λ (x) (f x)) x))))
; (λ (f) (λ (x) (f (f x))))
;
; Thus,
(define two
  ; Comparing `two` to `zero` we see that the effect of `f` on the final
  ; function is to be applied twice instead of zero times.  Or, comparing `two`
  ; to `one` we see the effect of `f` is to be applied one more time than in
  ; `one`.
  (lambda (f) (lambda (x) (f (f x)))))

; Let's assert that our derived definition for `one` and `two` are correct.
;
; To make these assertions, we need to compare the substitution method derived
; `one` and `two` to the corresponding procedurally derived `one-p` and
; `two-p`. Also because we cannot directly compare these procedures, at this
; point, we will compare the results of applying `one`, `one-p`, `two` and
; `two-p` to a function `f`,  where `f` is a λ function that adds 1 to a passed
; in parameter `x`:

(define (add-one)
  (lambda (x) (+ 1 x)))

(define f (add-one))

(define one-p (add-1 zero))

(define two-p (add-1 one-p))

(assert (= 1 ((one f) 0) ((one-p f) 0))
        "The substitution method derived `one` is equivalent to the procedurally derived `one-p`.")
(assert (= 2 ((two f) 0) ((two-p f) 0))
        "The substitution method derived `two` is equivalent to the procedurally derived `two-p`.")
(assert (= 0 ((zero f) 0))
        "`zero` nullifies the effect of #f.")

; The effect of procedure #add-1 on `one`, and `two` was the composition of `f` 
; onto `one` and `f` onto `two`.
;
; one   : (λ (f) (λ (x) (f x)))
; two   : (λ (f) (λ (x) (f (f x))))
; three : (λ (f) (λ (x) (f (f (f x)))))
;
; Thus one may surmise from this that an additive procedure in this system would
; work by composing one number onto the other.
;
; From exercise 1.42 you should already have a procedure called #compose.
;
; With a little trial and error (or just plain be able to see it) you get the
; following solution.
(define (adder n m)
  (lambda (f)
    (let ((nf (n f))
          (mf (m f)))
      (compose nf mf))))

(define three (adder one two))
(define three-r (adder two one))
(assert (= 3 ((three f) 0) ((three-r f) 0))
        "The #adder procedure can add numbers in our system..")

(define four (adder one three))
(define four-r (adder three one))
(define four-2 (adder two two))
(assert (= 4 ((four f) 0) ((four-r f) 0) ((four-2 f) 0))
        "The #adder procedure can add numbers in our system..")

; There are four properties of addition in the regular numerical system:
; communicative, associative, additive identity and distributive.  The
; following assertions will test the first three of these properties. Since the
; distributive property requires a multiplication operator, we will
; skip testing that property.

(assert (= (((adder two one) f) 0)  (((adder one two) f) 0))
        "#adder has communicative property because the order of `a` and `b`
        does not matter.")
(assert (= (((adder (adder three two) one) f) 0)  (((adder one (adder two three)) f) 0))
        "#adder has associative property because
        `(adder (adder one two) three)` is the same as
        `(adder one (adder two three)`.")
(assert (= ((four f) 0) (((adder zero four) f) 0) )
        "#adder has additive identity property because `(adder zero number)` is
        `number`.")

; {{{1 2.1.4 Extended Exercise: Interval Arithmetic (2.7 - 2.16)

; {{{4 Section procedures:
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

; {{{2 Exercise 2.7:
; {{{3 Problem
;      Alyssa's program is incomplete because she has not
;      specified the implementation of the interval abstraction.  Here is
;      a definition of the interval constructor:

   (define (make-interval a b) (cons a b))

;      Define selectors #upper-bound and #lower-bound to complete the
;      implementation.


; {{{3 Solution
(define lower-bound car)
(define upper-bound cdr)

(assert (= 1 (upper-bound (make-interval 0 1)))
        "Procedure #upper-bound works as expected.")

(assert (= 0 (lower-bound (make-interval 0 1)))
        "Procedure #lower-bound works as expected.")

; {{{2 Exercise 2.8:
; {{{3 Problem
;      Using reasoning analogous to Alyssa's, describe how the difference of
;      two intervals may be computed.  Define a corresponding subtraction
;      procedure, called #sub-interval.
; {{{3 Solution
;
; The upper bound of subtracting interval `b` from interval `a` should equal
; the value of subtracting the upper bound of interval `b` from the upper bound
; of interval `a`.  Similarly, the lower bound of the subtraction of interval
; `b` from interval `a` should be the subtraction of the lower bound of
; interval `b` from that of the lower bound of interval `a`.
;
(define (sub-interval a b)
  (make-interval (- (lower-bound a) (lower-bound b))
                 (- (upper-bound a) (upper-bound b))))

(assert (equal? (cons 0 0) (sub-interval (make-interval 1 2) (make-interval 1 2)))
        "Procedure #sub-interval works as expected.")

; {{{2 Exercise 2.9:
; {{{3 Problem
;      The "width" of an interval is half of the difference between its upper
;      and lower bounds.  The width is a measure of the uncertainty of the
;      number specified by the interval.  For some arithmetic operations the
;      width of the result of combining two intervals is a function only of the
;      widths of the argument intervals, whereas for others the width of the
;      combination is not a function of the widths of the argument intervals.
;      Show that the width of the sum (or difference) of two intervals is
;      a function only of the widths of the intervals being added (or
;      subtracted).  Give examples to show that this is not true for
;      multiplication or division.
;
; {{{3 Solution
(define (width x)
  (let ((lower (lower-bound x))
        (upper (upper-bound x)))
    (/ (- upper lower) 2)))

(assert (= .5 (width (make-interval 0 1)))
        "Procedure #width works as expected; for interval (0, 1).")

(assert (= 0 (width (make-interval 1 1)))
        "Procedure #width works as expected for interval (1, 1).")

; Let's first show that the width of the sum of two intervals is a function of
; the individual interval widths.
;
; By definition width = (upper - lower)/2
; let upper(x,y) = upper(x) + upper(y)
; let lower(x,y) = lower(x) + lower(y)
;
; Then,
; width(x, y) = (upper(x) + upper(y) - lower(x) + lower(y))/2
;             = (upper(x) - lower(x) + upper(y) - lower(y))/2
;             = (upper(x) - lower(x))/2 + (upper(y) - lower(y))/2
;             = with(x) + width(y)
; Likewise,
;
(assert (= (width (add-interval (make-interval 0 1)
                                (make-interval 2 3)))
           (+ (width (make-interval 0 1))
              (width (make-interval 2 3))))
        "The #width of the sum of two intervals is the sum of the component
        interval widths.")

; let upper(x,y) = upper(x) - upper(y)
; let lower(x,y) = upper(x) - upper(y)
;
; Then,
; width(x,y) = (upper(x) - upper(y) - lower(x) + lower(y))/2
;            = (upper(x) - upper(x) - lower(y) + lower(y))/2
;            = (upper(x) - upper(x))/2 - (lower(y) - lower(y))/2
;            = with(x) - width(y)
;
(assert (= (width (sub-interval (make-interval 0 1)
                                (make-interval 2 3)))
           (- (width (make-interval 0 1))
              (width (make-interval 2 3))))
        "The #width of the subtraction of two intervals is the subtraction of
        the component interval widths.")

; By example we can show that for multiplication the width is not a similar
; function of the widths for the individual interval components.
(assert (not (= (width (mul-interval (make-interval 0 1)
                                (make-interval 2 3)))
           (* (width (make-interval 0 1))
              (width (make-interval 2 3)))))
        "The #width of the multiplication of two intervals is not the
        multiplication of the component interval widths.")

; Likewise,
(assert (not (= (width (div-interval (make-interval 0 1)
                                (make-interval 2 3)))
           (/ (width (make-interval 0 1))
              (width (make-interval 2 3)))))
        "The #width of the division of two intervals is not the
        division of the component interval widths.")

; {{{2 Exercise 2.10:
; {{{3 Problem
;      Ben Bitdiddle, an expert systems programmer, looks over Alyssa's
;      shoulder and comments that it is not clear what it means to divide by an
;      interval that spans zero.  Modify Alyssa's code to check for this
;      condition and to signal an error if it occurs.
;
; {{{3 Solution
;
(define (div-interval x y)
  (let ((yu (upper-bound y))
        (yl (lower-bound y)))
    (if (or (= yu 0)
            (= yl 0))
      (error "Division by interval that spans zero is not possible.")
      (mul-interval x
                    (make-interval (/ 1.0 yu)
                                   (/ 1.0 yl))))))

(assert-error "Division by interval that spans zero is not possible."
              (lambda ()
                (div-interval (make-interval 1 2)
                              (make-interval 0 1)))
              "Error is thrown when division by zero is attempted.")

; {{{2 TODO Exercise 2.11:
; {{{3 Problem
;      In passing, Ben also cryptically comments: "By testing the signs of the
;      endpoints of the intervals, it is possible to break #mul-interval into
;      nine cases, only one of which requires more than two multiplications."
;      Rewrite this procedure using Ben's suggestion.
;
; {{{3 Solution
;
(define (mul-interval x y)
  (let ((xu (upper-bound x))
        (xl (lower-bound x))
        (yu (upper-bound y))
        (yl (lower-bound y)))
    (cond ((< xu xl yu yl))))
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; {{{2 TODO Exercise 2.12:
; {{{3 Problem
;      Define a constructor #make-center-percent that takes a center and
;      a percentage tolerance and produces the desired interval.  You must also
;      define a selector #percent that produces the percentage tolerance for
;      a given interval.  The #center selector is the same as the one shown
;      above.
; {{{3 Solution
;
; {{{2 TODO Exercise 2.13:
; {{{3 Problem
;      Show that under the assumption of small percentage tolerances there is
;      a simple formula for the approximate percentage tolerance of the product
;      of two intervals in terms of the tolerances of the factors.  You may
;      simplify the problem by assuming that all numbers are positive.
;
; {{{3 Solution
;
;
; {{{2 TODO Exercise 2.14:
; {{{3 Problem
;      After considerable work, Alyssa P. Hacker delivers her finished system.
;      Several years later, after she has forgotten all about it, she gets
;      a frenzied call from an irate user, Lem E. Tweakit.  It seems that Lem
;      has noticed that the formula for parallel resistors can be written in
;      two algebraically equivalent ways:
;
;      R1 * R2
;      -------
;      R1 + R2
;
;      and
;
;           1
;      -----------
;      1/R1 + 1/R2
;
;
;      He has written the following two programs, each of which computes the
;      parallel-resistors formula differently:
;
           (define (par1 r1 r2)
             (div-interval (mul-interval r1 r2)
                           (add-interval r1 r2)))

           (define (par2 r1 r2)
             (let ((one (make-interval 1 1)))
               (div-interval one
                             (add-interval (div-interval one r1)
                                           (div-interval one r2)))))
;
;      Lem complains that Alyssa's program gives different answers for the two
;      ways of computing. This is a serious complaint.
;
;      Demonstrate that Lem is right.  Investigate the behavior of the system
;      on a variety of arithmetic expressions.  Make some intervals `A` and
;      `B`, and use them in computing the expressions `A/A` and `A/B`.  You
;      will get the most insight by using intervals whose width is a small
;      percentage of the center value.  Examine the results of the computation
;      in center-percent form (see Exercise 2.12).
;
; {{{3 Solution
;
; {{{2 TODO Exercise 2.15:
; {{{3 Problem
;      Eva Lu Ator, another user, has also noticed the different intervals
;      computed by different but algebraically equivalent expressions. She says
;      that a formula to compute with intervals using Alyssa's system will
;      produce tighter error bounds if it can be written in such a form that no
;      variable that represents an uncertain number is repeated.  Thus, she
;      says, #par2 is a "better" program for parallel resistances than #par1.
;      Is she right?  Why?
;
; {{{3 Solution
;
; {{{2 TODO Exercise 2.16:
;
; {{{3 Problem
;      Explain, in general, why equivalent algebraic expressions may lead to
;      different answers.  Can you devise an interval-arithmetic package that
;      does not have this shortcoming, or is this task impossible?  (Warning:
;      This problem is very difficult.)
;
; {{{3 Solution
