
(define a (trap (me-loopforeach 1)))

(if (eq a '(exit-error eval_error))
    (print "SUCCESS")
  (print "FAILURE"))


