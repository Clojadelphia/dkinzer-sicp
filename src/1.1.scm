(declare (usual-integrations))
(load '("lib/assert" "lib/math"))

; Exercise 1.1
; Below is a sequence of expressions. 
; What is the result printed by the interpreter in response to each expression?
; Assume that the sequence is to be evaluated in the order in which it is presented.

(assert (= 10 10)
        "Numbers evaluate to numbers.")

(assert (= 12 (+ 5 3 4))
        "The + operator adds the operands.")

(assert (= 8 (- 9 1))
        "The - operator subtracts the rest of the operands from the first.")

(assert (= 3 (/ 6 2))
        "The / operator divides the first operand by rest of the operands.") 

(assert (= 8 (+ 2 (* 2 4) (- 4 6)))
        "S-expressions can contain sub-expressions.") 

(define a 3) ; a

(define b (+ a 1)) ; b

(assert (= 19 (+ a b (* a b)))
        "Defined symbols evaluate to their respective values.") 

(assert (not (= a b))
        "a is not equal to b.") 

(assert (= 4 (if (and (> b a) (< b (* a b)))
               b
               a))
        "The if form chooses.") 

(assert (= 16 (cond ((= a 4) 6)
            ((= b 4) (+ 6 7 a))
            (else 25)))
        "The cond form is a more general chooser.")


(assert (= 6 (+ 2 (if (> b a) b a)))
        "An if form can be used as an operand.")  

(assert (= 16 (* (cond ((> a b) a)
         ((< a b) b)
         (else -1)) 4))
        "A cond form can be used as an operand.") 


; Exercise 1.2
; Translate the following expression into prefix form.
; 5 + 1/2 + (2 - (3 - (6 + 1/3) )) / 3 * (2 - 6) (2 - 7)
(assert (= (/ 13 72)
           (/ (+ 5 (/ 1 2) (- 2 (- 3 (+ 6 (/ 1 3)))))
   (* 3 (- 2 6) (- 2 7))))
        "These nested expressions can get pretty Freaky.")

;;;SECTION 1.1.4
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

; Exercise 1.3
; Define a procedure that takes three numbers as arguments and returns
; the sum of the squares of the two larger numbers.
(define exercise-1.3
  (lambda (x y z)
    (define sum-of-squares
      (lambda (x y) (+ (square x) (square y))))
    (cond ((< x y z) (sum-of-squares y z))
          ((< z y x) (sum-of-squares x y))
          (else (sum-of-squares x z)))))

(assert (= 13 (exercise-1.3 1 2 3))
        "Exercise-1.3 works when the last 2 operands are greater.")

(assert (= 13 (exercise-1.3 3 2 1))
        "Exercise-1.3 works when the first 2 operands are greater.")

(assert (= 13 (exercise-1.3 3 1 2))
        "Exercise-1.3 works when the exterior operands are greater.")

; Exercise 1.4
; Observe that our model of evaluation allows for combinations whose operators are
; compound expressions. Use this observation to describe the behavior of the following
; procedure:

(define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b))
(assert (= 2 (a-plus-abs-b 1 -1) (a-plus-abs-b 1 1))
        "S-Expressions that evaluate to operators may take the place of an operator.
        In this case the operator behaves differently dependent on the value of the
        second argument.")

; Exercise 1.5
; Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is
; using applicative-order evaluation or normal-order evaluation. He defines the following two
; procedures:
(define (p)
  (display
    "TEST: This message will only display under applicative-order
    evaluation."))

(define (test x y)
  (if (= x 0)
      0
      y))
; Then he evaluates the expression
;
; What behavior will Ben observe with an interpreter that uses
; applicative-order evaluation? What behavior will he observe with an
; interpreter that uses normal-order evaluation? Explain your answer. (Assume
; that the evaluation rule for the special form if is the same whether the
; interpreter is using normal or applicative order: The predicate expression is
; evaluated first, and the result determines whether to evaluate the consequent
; or the alternative expression.)
(assert (= 0 (test 0 (p)))
        "The rule for normal normal-order evaluation dictates that the operands
        of an expression are never to be evaluated until all expressions have
        been fully expanded.  Therefore presented with the test in exercise 1.5
        one may assume that the normal oder evaluation would fall into an
        infinite loop when trying to expand the second operand:

        (p (p(p (p (p ..)))))

        However, assuming the (if) expression is evaluated in the same way
        under either situations, i.e. the full expansion rule is broken for
        (if), Then the second operand (p) would never get evaluated under
        normal-order evaluation, but would under applicative-order evaluation
        because the rule for applicative-order evaluation is that all operands
        get evaluated first before the operator is applied.

        Note: a less destructive test would be to test for a side-effect such
        printing an output.  I have changed the test accordingly in order to be
        able to run it without falling into an infinite loop situation.")

; Exersise 1.6
; Alyssa P. Hacker doesn't see why if needs to be provided as a
; special form. ``Why can't I just define it as an ordinary procedure in terms of
; cond?'' she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done,
; and she defines a new version of if:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else
          else-clause)))

; Eva demonstrates the program for Alyssa:

(assert (= 5 (new-if (= 2 3) 0 5))
        "new-if works as expected for #f predicates.")

(assert (= 0 (new-if (= 1 1) 0 5))
        "new-if works as expected for #t predicates.")

; Delighted, Alyssa uses new-if to rewrite the square-root program:


(define (sqrt-iter guess x)

  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

  (define (improve guess x)
    (average guess (/ x guess)))

  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define exercise-1.6
  (lambda ()
    (display (string "TEST: exercise-1.6 would not have been evaluated by a real (if) expression."))))

; What happens when Alyssa attempts to use this to compute square roots? Explain.
(assert (= 0 (new-if (= 1 1) 0 (exercise-1.6)))
        "exercise-1.6 is evaluated inside of (new-if) because (new-if) is not a special-form.
        Therefore, when Alyssa attempts to compute a square using (new-if), the procedure will
        fall into an infinite evaluation loop because all operands are evaluated for normal expressions.
        Since sqrt-iter is recursive, the break operand will never get a chance to be evaluated.")

(assert (= 0 (if (= 1 1) 0 (exercise-1.6)))
        "exercise-1.6 is not evaluated  in a real (if) expression (i.e. no side effects is possible).")

; Exercise 1.7a
; The good-enough? test used in computing square roots will not be very effective
; for finding the square roots of very small numbers. Also, in real computers,
; arithmetic operations are almost always performed with limited precision. This
; makes our test inadequate for very large numbers. Explain these statements,
; with examples showing how the test fails for small and large numbers.

(define sqrt-1.7a
  (lambda (x)
    (define (good-enough? guess)
      (if (= x 1e13)
        (< (abs (- (square guess) x)) 0.01)
        (< (abs (- (square guess) x)) 0.001))
      )
    (define (improve guess)
      (average guess (/ x guess)))
    (define (sqrt-iter guess)
      (if (good-enough? guess)
        ((lambda ()
           (display (string "TEST: guess -> " guess ", " "x -> " x "\n"))
           guess))
        (sqrt-iter (improve guess))))
    (sqrt-iter 1.0)))

(assert (= .03125 (sqrt-1.7a 1e-100) (sqrt-1.7a 1e-101))
        "sqrt-1.7a fails for very small numbers.")

(assert (and (= 0 (- (+ 1e13 .002) (+ 1e13 .001)))
             (<= 0.001 (- (+ 1e13 .02) (+ 1e13 .01))))
        "This machine cannot distinguish differences of <= .001 for operations
        involving numbers <= 1e13.")

(assert (> .01 (abs (- (square (sqrt-1.7a 1e13)) 1e13)))
        "sqrt-1.7a fails for very large  numbers.")

; Exercise 1.7b
; An alternative strategy for implementing good-enough? is to watch how guess
; changes from one iteration to the next and to stop when the change is a very
; small fraction of the guess. Design a square-root procedure that uses this
; kind of end test. Does this work better for small and large numbers?

(define sqrt-1.7b
  (lambda (x)
    (define (good-enough? guess new-guess)
      (> 1.0e-100 (abs (- guess new-guess))))
    (define (improve guess)
      (average guess (/ x guess)))
    (define (sqrt-iter guess)
      (define new-guess (improve guess))

      (if (good-enough? guess new-guess)
        ((lambda ()
           (display (string "TEST: guess -> " guess ", " "x -> " x "\n"))
           guess))
        (sqrt-iter new-guess)))
    (sqrt-iter 1.0)))

(assert (and 
          (> 1e-50 (abs (- (square (sqrt-1.7b 1.0e-51)) 1.0e-51)))
          (not (= (sqrt-1.7b 1e-100) (sqrt-1.7b 1e-101)))) 
        "sqrt-1.7b works on extremely small integers. ")

(assert (> 1e-50 (abs (- (square (sqrt-1.7b 1e25)) 1e25)))
        "sqrt-1.7b works on very large integers.")

; Exercise 1.8 Newton's method for cube roots is based on the fact that if y is
; an approximation to the cube root of x, then a better approximation is given
; by the value
;
; (x/y^2 + 2y)/3
;
; Use this formula to implement a cube-root procedure analogous to the
; square-root procedure. (In section 1.3.4 we will see how to implement
; Newton's method in general as an abstraction of these square-root and
; cube-root procedures.)

(define cbrt-1.8
  (lambda (x)
    (define (good-enough? guess new-guess)
      (> 1.0e-100 (abs (- guess new-guess))))
    (define (improve guess)
      (/ (+ (/ x (square guess)) (* 2 guess)) 3))
    (define (cbrt-iter guess)
      (define new-guess (improve guess))
      (if (good-enough? guess new-guess)
        guess
        (cbrt-iter new-guess)))
    (cbrt-iter 1.0)))

(assert (= 3 (cbrt-1.8 27))
        "cbrt-1.8 works for a known case.")

(assert (and
          (> 1e-50 (abs (- (cube (cbrt-1.8 1.0e-51)) 1.0e-51)))
          (not (= (cbrt-1.8 1e-100) (cbrt-1.8 1e-101))))
        "cbrt-1.8 works on extremely small integers. ")

(assert (> 1e-50 (abs (- (cube (cbrt-1.8 1e25)) 1e25)))
        "cbrt-1.8 works on very large integers.")
