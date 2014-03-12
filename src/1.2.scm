(declare (usual-integrations))
(load '("lib/assert" "lib/math"))

; Exercise 1.9
; Each of the following two procedures defines a method for adding two positive
; integers in terms of the procedures inc, which increments its argument by 1,
; and dec, which decrements its argument by 1.

(define (+1.9a a b)
  (if (= a 0)
    b
    (inc (+1.9a (dec a) b))))

(define (+1.9b a b)
  (if (= a 0)
    b
    (+1.9b (dec a)  (inc b))))

(set-gc-notification! #t)

(display (string "TEST time for : (+1.9a 1e4 1e4)"))
(define +1.9a-times
  (time (lambda () (+1.9a 1e4 1e4))))

(display (string "TEST time for : (+1.9b 1e4 1e4)"))
(define +1.9b-times
  (time (lambda () (+1.9b 1e4 1e4))))

(assert (= +1.9a-times +1.9b-times)
        "There are no time differences between procedure +1.9a and +1.9b.")
