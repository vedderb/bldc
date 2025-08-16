
(define a (trap (/ 1 0.0f64)))
  
(if (eq a '(exit-error division_by_zero))
    (print "SUCCESS")
  (print "FAILURE"))
