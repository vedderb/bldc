
(define r1 (eq '(exit-error type_error) (trap (setassoc 1 2))))
(define r2 (eq '(exit-error type_error) (trap (setassoc 1))))

(if (and r1 r2)
    (print "SUCCESS")
    (print "FAILURE"))
