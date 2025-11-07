

(define a (trap (/ 1 0.0)))
  
(if (eq a '(exit-error division_by_zero))
    (print "SUCCESS")
  (print "FAILURE"))
