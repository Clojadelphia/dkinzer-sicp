(define assert
  (lambda (a m)
    (if a 
      (display (string  "PASSED: " m "\n"))
      (display (string   "FAILED: " m "\n")))))
