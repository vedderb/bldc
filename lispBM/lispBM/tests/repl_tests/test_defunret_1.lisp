

(define a (trap (me-defunret 1)))

(if (eq a '(exit-error eval_error))
    (print "SUCCESS")
  (print "FAILURE"))


