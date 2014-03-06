; @file
; assert.scm
; Testing and analysis type functions go here.

(define assert
  (lambda (a m)
    (if a 
      (display (string "\n" "PASSED: " m "\n"))
      (display (string "\n" "FAILED: " m "\n")))))

; Use (time f) to analyse how long procedures take.
; f is an unevaluated expression.
(define time
  (lambda (f)
    (define t (process-time-clock))
    (f)
    (- (process-time-clock) t)))
