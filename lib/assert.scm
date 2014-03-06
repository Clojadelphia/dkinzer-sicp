; @file
; assert.scm
; Testing and analysis type functions go here.

(define assert
  (lambda (a m)
    (if a 
      (display (string "\n" "PASSED: " m "\n"))
      (display (string "\n" "FAILED: " m "\n")))))

; Use (time f) to analyse how long procedures take.
; f is an unevaluated expression. run-time gc-time
; & real-time are written to output and returned as a list.
(define time
  (lambda (f)
    (with-timings
      f
      (lambda (run-time gc-time real-time)
      (newline)
      (write run-time)
      (write-char #\space)
      (write gc-time)
      (write-char #\space)
      (write real-time)
      (newline)
      (list run-time gc-time real-time)))))
