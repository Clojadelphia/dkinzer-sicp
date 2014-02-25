; Problem 1.1.Exercise 1.1.  Below is a sequence of expressions. 
; What is the result printed by the interpreter in response to each expression?
; Assume that the sequence is to be evaluated in the order in which it is presented.
(define assert
  (lambda (a m)
    (if a 
      (display (string  "PASSED: " m "\n"))
      (display (string   "FAILED: " m "\n")))))

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

(assert (= 15 (* (cond ((> a b) a)
         ((< a b) b)
         (else -1)) 4))
        "A cond form can be used as an operand.") 
