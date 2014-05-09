(declare (usual-integrations))

;;;;CODE FROM STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS
;;;;Any useful expression from SICP will go here.

;;; {{{1 SECTION 1.1
(define (square x) (* x x))

(define (cube x) (* x x x))

(define (average x y)
    (/ (+ x y) 2))

;;; {{{1 SECTION 1.2
(define inc (lambda (x)
              (+ x 1)))

(define dec (lambda (x)
              (- x 1)))

(define (gcd a b)
  ; Greatest Common Divisor.
  (if (= b 0)
    a
    (gcd b (remainder a b))))

;;; {{{1 SECTION 1.3
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (pi-div-4 x)
  (define (term y)
    (let ((even-term (* 2 y))
          (odd-term (+ 1 (* 2 y))))
      (let ((first-factor (/ even-term odd-term))
            (second-factor (/ (+ 2 even-term) odd-term)))
       (*  first-factor second-factor))))
  (product term 1.0 inc x))

(define PI (* 4 (pi-div-4 100)))

(define (prime? x)
  (define (iter a)
   (if (or (= 0 a) (= 1 a) (= x a))
    #t
    (if (= 0 (modulo x a))
      #f
      (iter (inc a)))))
  ; We want to start with two in order to quickly evaluate even integers as non primes.
  (iter 2))

(define (average-damp f)
    (lambda (x)  (average x (f x))))

(define dx .00001)

;;; {{{1  SECTION 2.1
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
