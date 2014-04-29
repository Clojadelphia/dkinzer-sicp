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
   (if (= a x)
    #t
    (if (= 0 (modulo x a))
      #f
      (iter (inc a)))))
  ; We want to start with two in order to quickly evaluate even integers as non primes.
  (cond ((< x 2) #f)
        ((= x 2) #t)
        (else (iter 2))))

(assert (equal? '(2 3 5 7) (filter prime? '( -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10)))
        "prime? works as expected.")
; a. the sum of the squares of the prime numbers in the interval a to b

;; General form of the accumulate procedure.
(define (filtered-accumulate combiner null-value term mask a next b)
  (define (filtered-term y)
    (term (if (mask y)
             y
             null-value)))
  (if (> a b)
    null-value
    (combiner (filtered-term a) (filtered-accumulate combiner null-value term mask (next a) next b))))

(define (sum-squares-primes a b)
  (filtered-accumulate + 0 square prime? a inc b))

(assert (= 0 (sum-squares-primes 0 0))
        "sum-squares-primes works when 0 0 is passed.")
(assert (= 0 (sum-squares-primes 0 1))
        "sum-squares-primes works when 0 1 is passed.")
(assert (= 0 (sum-squares-primes 1 1))
        "sum-squares-primes works when 1 1 is passed.")
(assert (= 0 (sum-squares-primes -1 1))
        "sum-squares-primes works when -1 -1 is passed.")
(assert (= (+ 4 9 25) (sum-squares-primes -1 5))
        "sum-squares-primes works when -1 5 is passed.")
(assert (= (+ 4 9 25 49) (sum-squares-primes -1 7))
        "sum-squares-primes works when -1 7 is passed.")

; b. the product of all the positive integers less than n that are relatively
; prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).

;; Returns a list of integers from 1 to n.
(define (range n)
  (define (iter a result)
    (if (<= a 0)
      result
      (iter (dec a) (cons a result))))
  (iter n '()))

(assert (equal? '() (range -1))
        "range works as expected when passed -1")
(assert (equal? '() (range 0))
        "range works as expected when passed 0")
(assert (equal? '(1) (range 1))
        "range works as expected when passed 1")
(assert (equal? '(1 2 3 4 5 6) (range 6))
        "range works as expected when passed 6")

;; Return the list of factors for a given integer "a".
(define (factors a)
  (define (factor? b)
    (if (= 0 (modulo a b))
      #t
      #f))
  (let ((primes (filter prime? (range a))))
    (let ((f (filter factor? primes)))
      f)))

(assert (equal? '() (factors -1))
        "factor works as expected for x less than 0.")
(assert (equal? '() (factors 0))
        "factor works as expected for x equal 0.")
(assert (equal? '() (factors 1))
        "factor works as expected for x equal 1.")
(assert (equal? '(2) (factors 2))
        "factor works as expected for x equal 2.")
(assert (equal? '(2 3) (factors 6))
        "factor works as expected for x equal 6.")

;; Check that integer "a" is a relative prime of integer "b".
(define (relative-prime? a b)
  (define (abs-modulo a b)
    (if (< a b)
      (modulo b a)
      (modulo a b)))
  (define (iter a result)
    (if (= 0 (length a))
      (= 0 (length result))
      (if (= 0 (abs-modulo b (car a)))
        (iter (cdr a) (cons (car a) result))
        (iter (cdr a) result))))
  (iter (factors a) (list)))

(assert (= 0 (length '()))
        "The length of '() is 0.")
(assert (equal? '(1) (cons 1 '()))
        "cons works as expected")
(assert (= (modulo 2 3) (modulo (car '(2)) 3))
        "modulo works as expected")
(assert (relative-prime? 10 9)
        "relative-prime? works as expected when 9, 10 is passed.")
(assert (relative-prime? 9 10)
        "relative-prime? does not care about order (10, 9).")
(assert (relative-prime? 7 11)
        "relative-prime? works when two primes are passed.")
(assert (not (relative-prime? 2 100))
        "relative-prime? works when non-relative primes are passed.")
(assert (not (relative-prime? 100 2))
        "relative-prime? works when non-relative primes are passed.
        In any order.")
(assert (relative-prime? 1 11)
        "relative-prime? does not bonk when a 1 attribute is given.")

; b. the product of all the positive integers less than n that are relatively
; prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).
(define (product-of-relative-primes-to n)
  (let ((term (lambda (a) a))
        (mask (lambda (a) (relative-prime? a n))))
    (filtered-accumulate * 1 term mask 1 inc n)))

(assert (= (* 1 2 3 4 5 6) (product-of-relative-primes-to 7))
        "#product-of-relative-primes-to works as expected for input 7 (a prime)")
(assert (= (* 1 2 4 5 7 8) (product-of-relative-primes-to 9))
        "#product-of-relative-primes-to works as expected for input 9 (an odd non prime)")
(assert (= (* 1 3 7 9) (product-of-relative-primes-to 10))
        "#product-of-relative-primes-to works as expected for input 10 (an even non prime)")
(assert (= (* 1 5 7 11) (product-of-relative-primes-to 12))
        "#product-of-relative-primes-to works as expected for input 12 (an even non prime with both odd and even primes)")

; Exercise 1.34:
; Suppose we define the procedure
(define (f g)
  (g 2))
; Then we have
; (f square)
; 4
; (f (lambda (z) (* z (+ z 1))))
; 6

; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.
(assert-error "The object 2 is not applicable"
              (lambda () (f f))
              "(f f) -> (f (f 2) -> (f (f (2 2):
              The form expands until it tries to apply 2 at which point it throws an error.")

; Exercise 1.35:
; Show that the golden ratio  (section 1.2.2) is a fixed point
; of the transformation x   1 + 1/x, and use this fact to compute  by means of
; the fixed-point procedure.
;
; P satisfies          P^2 = P + 1
; which transforms to  P = 1 + 1/P
; which implies:       P -> 1 + 1/P
; or P is the fixpoint of 1 + 1/P
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (newline)
  (display (string "TEST: display fixed-point results."))
  (define (close-enough? v1 v2)
    (newline)
    (display v1)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define phi
  (fixed-point (lambda (p) (+ 1 (/ 1 p)))
             1.0))
(assert (< .00001 (- phi 1.6180))
        "The fixed point procedure can be used to estimate the Golden Ratio.")

; Exercise 1.36:
; Modify fixed-point so that it prints the sequence of
; approximations it generates, using the newline and display primitives shown
; in exercise 1.22. Then find a solution to
;
; x^x = 1000
;
; by finding a fixed point of
;
; x -> log(1000)/log(x).
;
; (Use Scheme's primitive log procedure, which
; computes natural logarithms.) Compare the number of steps this takes with and
; without average damping. (Note that you cannot start fixed-point with a guess
; of 1, as this would cause division by log(1) = 0.)
(define (ex-1.36a)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               2.0))
(define (ex-1.36b)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
               2.0))
(assert (< .0000000001 (abs (- (ex-1.36b) (ex-1.36a))))
        "Fixed point with or without dampening is equivalent.")

; Exercise 1.37:
; a. An infinite continued fraction is an expression of the
; form
;
;            N1
;  f =  ------------
;       D1   +    N2
;             ----------
;             D2  +   N3
;                     --------
;                     D3 + ...
;
; As an example, one can show that the infinite continued fraction expansion
; with the Ni and the Di all equal to 1 produces 1/, where  is the golden ratio
; (described in section 1.2.2). One way to approximate an infinite continued
; fraction is to truncate the expansion after a given number of terms. Such
; a truncation -- a so-called k-term finite continued fraction -- has the form
;
;            N1
;  f =  ------------
;       D1   +    N2
;             -------------
;             D2  +  ... Nk
;                       ----
;                        Dk
;
; Suppose that n and d are procedures of one argument (the term index i) that
; return the Ni and Di of the terms of the continued fraction. Define
; a procedure cont-frac such that evaluating (cont-frac n d k) computes the
; value of the k-term finite continued fraction. Check your procedure by
; approximating 1/Phi using
;
(define (cont-frac n d k)
  (define (frac i)
    (if (= i k)
      (/ (n k) (d k))
      (/ (n i) (+ (d i) (frac (inc i))))))
  (frac 1))

(define (inverse-phi k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

; for successive values of k. how large must you make k in order to get an
; approximation that is accurate to 4 decimal places?
(assert (> .0001 (abs (- (/ 1.0000 phi) (inverse-phi 10))))
        "The conc-frac procedure can be used to estimate the 1/phi.
         k must be set to 10 in order to bring the aproximation
         accurate to 4 decimal places.")

; b. If your cont-frac procedure generates a recursive process, write one that
; generates an iterative process.  If it generates an iterative process, write
; one that generates a recursive process.
(define (cont-frac-i n d k)
  (display "TEST: cont-frac-i results")
  (newline)
  (define (frac-iter i result)
    (display (string i ": " result))
    (newline)
    (if (= i 0)
      result
      (frac-iter (dec i)
                 (/ (n i) (+ (d i) result)))))
  (frac-iter k 0))

(define (inverse-phi-i k)
  (cont-frac-i (lambda (i) 1.0)
               (lambda (i) 1.0)
               k))

(assert (= (inverse-phi 10) (inverse-phi-i 10))
        "The recursive and iterative versions of cont-frac are equivalent.
        The iterative version required more thinking becuase I needed to reverse
        the order of the calculations.")

; Exercise 1.38:
; In 1737, the Swiss mathematician Leonhard Euler published
; a memoir De Fractionibus Continuis, which included a continued fraction
; expansion for e - 2, where e is the base of the natural logarithms. In this
; fraction, the Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1,
; 6, 1, 1, 8, .... Write a program that uses your cont-frac procedure from
; exercise 1.37 to approximate e, based on Euler's expansion
(define (e-minus-2 k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i)
               (cond ((= 1 i) 1.0)
                     ((= 2 i) 2.0)
                     ((= 0 (modulo (- i 2) 3)) (* 2.0 (/ (+ i 3.0) 3.0)))
                     (else 1.0)))
             k))

(define e (+ 2 (e-minus-2 10)))

(assert (> .001 (abs (- 2.71828 e)))
        "The conc-frac procedure can be used to etimate the natural number e.")

; Exercise 1.39:
; A continued fraction representation of the tangent function
; was published in 1770 by the German mathematician J.H. Lambert:
;
;             x
;  tan x = -------
;          1  -  x^2
;             ---------
;             3  -  x^2
;                   --------
;                    5 - ...
;
; where x is in radians. Define a procedure (tan-cf x k) that computes an
; approximation to the tangent function based on Lambert's formula. K specifies
; the number of terms to compute, as in exercise 1.37.

(define (cont-frac-n n d k)
  (define (frac i)
    (if (= i k)
      (/ (n k) (d k))
      (/ (n i) (- (d i) (frac (inc i))))))
  (frac 1))

(define (tan-cf x k)
  (cont-frac-n (lambda (i)
               (if (= 1 i)
                 x
                 (square x)))
             (lambda (i) (- (* 2.0 i) 1))
             k))

; Can con-frac? be written in the form of the accumulate procedure?
(assert (= -2.185039863261519 (tan-cf 2 25))
        "A cont-frac procedure can be used to estimate tan x.")

; Exercise 1.40:
; Define a procedure cubic that can be used together with the newtons-method
; procedure in expressions of the form
;
; (newtons-method (cubic a b c) 1)
;
; to approximate zeros of the cubic x^2 + ax^2 + bx + c.
(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (is-cubic-root? x g)
  ; Where g is in the form of (cubic a b c)
  (=
    (newtons-method g x)
    x))

;; We test is-cubit-root? against the following cubic equation
;; (x + 1) (x + 2) (x + 3) = x^3 + 6x^2 + 11x + 6
(assert (is-cubic-root? -1 (cubic 6 11 6))
        "-1 is a root of x^3 + 6x^2 + 11x + 6")

(assert (is-cubic-root? -2 (cubic 6 11 6))
        "-2 is a root of x^3 + 6x^2 + 11x + 6")

(assert (is-cubic-root? -3 (cubic 6 11 6))
        "-3 is a root of x^3 + 6x^2 + 11x + 6")
