
(define e1 (trap (atomic (recv))))

(if (eq '(exit-error eval_error) e1)
    (print "SUCCESS")
    (print "FAILURE"))
