(define assert
  (lambda (a m)
    (if a 
      (display (string "\n" "PASSED: " m "\n"))
      (display (string "\n" "FAILED: " m "\n")))))
