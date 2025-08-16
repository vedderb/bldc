

(define a (trap (me-defun 1)))

(if (eq a '(exit-error eval_error))
    (print "SUCCESS")
  (print "FAILURE"))


