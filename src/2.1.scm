(declare (usual-integrations))
(load '("lib/assert" "lib/math"))

; {{{1 2.1.1 Example: Arithmetic Operations for Rational Numbers
; {{{2 Exercise 2.1:
; Problem
;      Define a better version of make-rat that handles both positive and
;      negative arguments.  make-rat should normalize the sign so that if the
;      rational number is positive, both the numerator and denominator are
;      positive, and if the rational number is negative, only the numerator is
;      negative.
;
; {{{3 Solution

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

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
