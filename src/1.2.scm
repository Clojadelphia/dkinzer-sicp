(load '("lib/assert" "lib/math"))

; Exercise 1.9
; Each of the following two procedures defines a method for adding two positive
; integers in terms of the procedures inc, which increments its argument by 1,
; and dec, which decrements its argument by 1.

 (define (+1.9a a b)
   (if (= a 0)
     b
     (inc (+ (dec a) b))))

 (define (+1.9b a b)
   (if (= a 0)
     b
    (+ (dec a)  (inc b))))
(set-gc-notification! #t)

(display (string "TEST time for : (+1.9a 10000000000000.0e100 10000e100)"))
(define +1.9a-times
  (time (lambda () (+1.9a 10000000000000.0e100 10000e100))))

(display (string "TEST time for : (+1.9b 10000000000000.0e100 10000e100)"))
(define +1.9b-times
  (time (lambda () (+1.9b 10000000000000.0e100 10000e100))))

(assert (= +1.9a-times +1.9b-times)
        "There are no time differences between procedure +1.9a and +1.9b.")
