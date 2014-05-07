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
