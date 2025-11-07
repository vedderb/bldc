
(define r1 (trap (wait)))
(define r2 (trap (wait 'apa)))

(if (and (eq r1 '(exit-error type_error))
         (eq r2 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))
