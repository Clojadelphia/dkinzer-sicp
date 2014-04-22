(declare (usual-integrations))

;;;;CODE FROM STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS
;;;;Any useful expression from SICP will go here.

;;;SECTION 1.1
(define (square x) (* x x))

(define (cube x) (* x x x))

(define (average x y)
    (/ (+ x y) 2))

;;;SECTION 1.2
(define inc (lambda (x)
              (+ x 1)))

(define dec (lambda (x)
              (- x 1)))

;;; SECTION 1.3
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
