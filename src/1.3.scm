(declare (usual-integrations))
(load '("lib/assert" "lib/math"))

; Exercise 1.29:
; Simpson's Rule is a more accurate method of numerical integration than the
; method illustrated above.  Using Simpson's Rule, the integral of
; a function $f$ between $a$ and $b$ is approximated as

; h/3 [ y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_{n-2} + 4y_{n-1} + y_n ]

; where $h = (b - a)/n$, for some even integer $n$, and $y_k = f(a + kh)$.
; (Increasing $n$ increases the accuracy of the approximation.)  Define
; a procedure that takes as arguments $f$, $a$, $b$, and $n$ and returns the
; value of the integral, computed using Simpson's Rule.  Use your procedure to
; integrate =cube= between 0 and 1 (with $n = 100$ and $n = 1000$), and
; compare the results to those of the =integral= procedure shown above.

(define (sympson-integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (y k)
      (let ((m (cond ((or (= k 0) (= k n)) 1)
                     ((odd? k) 4)
                     ((even? k) 2))))
        (* m (f (+ a (* k h))))))
    (* (/ h 3) (sum y 0.0 inc n))))

(assert (= (sympson-integral cube 0 1 100) .24999999999999992)
        "sympson-integral of cube works as expected.")

; Exercise 1.30:
; The =sum= procedure above generates a linear recursion.  The procedure can be
; rewritten so that the sum is performed iteratively.  Show how to do this by
; filling in the missing expressions in the following definition:
(define (sum-130 term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(assert (= (sum-130 cube 1 inc 10) (sum cube 1 inc 10))
        "The iterative procedure sum-130 is equivalent recursive procedure sum.")

; Exercise 1.31:
; a. The sum procedure is only the simplest of a vast number of
; similar abstractions that can be captured as higher-order procedures.51 Write
; an analogous procedure called product that returns the product of the values of
; a function at points over a given range. Show how to define factorial in terms
; of product. Also use product to compute approximations to  using the formula:
;
; pi/4 = (2/3 * 4/3) * (4/5 * 6/5) * (6/7 * 8/7) * ... (2n/(2n+1) * (2n+2)/(2n+1))
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial x)
  (let ((term (lambda (x) x))
        (next inc)
        (a 1)
        (b x))
    (product term a next b)))

(assert (= 1 (factorial 0)) "0 factorial is 1")
(assert (= 1 (factorial 1)) "1 factorial is 1")
(assert (= 2 (factorial 2)) "2 factorial is 2")
(assert (= 6 (factorial 3)) "3 factorial is 6")

(define (pi-div-4 x)
  (define (term y)
    (let ((even-term (* 2 y))
          (odd-term (+ 1 (* 2 y))))
      (let ((first-factor (/ even-term odd-term))
            (second-factor (/ (+ 2 even-term) odd-term)))
       (*  first-factor second-factor))))
  (product term 1.0 inc x))

(define PI (* 4 (pi-div-4 100)))

(assert (< 0.00000000000001 (abs (- 3.1415 PI)))
        "Using pi-div-4 we can closely approximate pi.")

; One of the things that really caught me with the above statement is that in
; an mit-scheme let form, the previously bound variable cannot be directly
; reused in defining the next bound variable.
;
; For example, the following form throws an error.
;
; (let ((a 1)
;       (b (+ a 1)))
;   a)
;
; b. If your product procedure generates a recursive process, write one that
; generates an iterative process. If it generates an iterative process, write one
; that generates a recursive process.
;
(define (product-recur term a next b)
  ;; A recursive version of the product procedure.
  (if (> a b)
    1
    (* (term a) (product-recur term (next a) inc b))))

(define (factorial-recur x)
  (product-recur (lambda (x) x) 1 inc x))

(assert (= (factorial-recur 10) (factorial 10))
        "The product and product-recur procedures are equivalent.")

; Exercise 1.32:
; a. Show that sum and product (exercise 1.31) are both
; special cases of a still more general notion called accumulate that
; combines a collection of terms, using some general accumulation function:

; (accumulate combiner null-value term a next b)

; Accumulate takes as arguments the same term and range specifications as sum
; and product, together with a combiner procedure  (of two arguments) that
; specifies how the current term is to be combined with the accumulation of the
; preceding terms and a null-value that specifies what base value to use
; when the terms run out. Write accumulate and show how sum and product can
; both be defined as simple calls to accumulate.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum-132a term a next b)
  (accumulate + 0 term a next b))

(define (sum-130-integers a b)
  (sum-130 (lambda (x) x) a inc b))

(define (sum-132a-integers a b)
  (sum-132a (lambda (x) x) a inc b))

(assert (= (sum-130-integers 0 100) (sum-132a-integers 0 100))
        "sum procedures can be re-factored to use the more general accumlate procedure.")

(define (product-132a term a next b)
  (accumulate * 1 term a next b))

(define (product-recur-integers a b)
  (product-recur (lambda (x) x) a inc b))

(define (product-132a-integers a b)
  (product-132a (lambda (x) x) a inc b))

(assert (= (product-recur-integers 0 100) (product-132a-integers 0 100))
        "product procedures can be re-factored to use the more general accumlate procedure.")

; b. If your accumulate procedure generates a recursive process, write one
; that generates an iterative process. If it generates an iterative
; process, write one that generates a recursive process.
(define (accumulate-iter combiner null-value term a next b)
  (define (iter result x)
    (if (> x b)
      result
      (iter (combiner result (term x)) (next x))))
  (iter null-value a))

(define (product-132b term a next b)
  (accumulate-iter * 1 term a next b))

(define (product-132b-integers a b)
  (product-132b (lambda (x) x) a inc b))

(assert (= (product-132a-integers 1 4) (product-132b-integers 1 4))
        "The iterative and recursive forms of the accumulater procedures are equivalent.")

; Exercise 1.33:
; You can obtain an even more general version of accumulate (exercise 1.32) by
; introducing the notion of a filter on the terms to be combined. That is,
; combine only those terms derived from values in the range that satisfy
; a specified condition. The resulting filtered-accumulate abstraction takes the
; same arguments as accumulate, together with an additional predicate of one
; argument that specifies the filter. Write filtered-accumulate as a procedure.
; Show how to express the following using filtered-accumulate:
;
(define (prime? x)
  (define (iter a)
   (if (or (= 0 a) (= 1 a) (= x a))
    #t
    (if (= 0 (modulo x a))
      #f
      (iter (inc a)))))
  ; We want to start with two in order to quickly evaluate even integers as non primes.
  (iter 2))

(assert (equal? '(3 5 7) (filter prime? '(3 4 5 6 7 8 9)))
        "prime? works as expected.")
; a. the sum of the squares of the prime numbers in the interval a to b
(define (filtered-accumulate combiner null-value term mask a next b)
  (define (filtered-term y)
    (if (mask y)
      null-value
      (term y)))

  (if (> a b)
    null-value
    (combiner (filtered-term a) (filtered-accumulate combiner null-value mask term (next a) next b))))

(define (sum-squares-primes a b)
  (filtered-accumulate * 1 square prime? a inc b))

; b. the product of all the positive integers less than n that are relatively
; prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).
