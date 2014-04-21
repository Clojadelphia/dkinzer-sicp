(declare (usual-integrations))

;;;;CODE FROM STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS
;;;;Any useful expression from SICP will go here.

;;;SECTION 1.1.4


(define (square x) (* x x))

(define (cube x) (* x x x))

;;;SECTION 1.1.7
(define (average x y)
    (/ (+ x y) 2))

;;;SECTION 1.2.1
(define inc (lambda (x)
              (+ x 1)))

(define dec (lambda (x)
              (- x 1)))

;;; SECTION 1.3.1
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
