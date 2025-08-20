

(define r1 (eq '(exit-error type_error) (trap (undefine 1 2))))
(define r2 (eq '(exit-error type_error) (trap (undefine))))

(define r3 (undefine '(apa bepa cepa)))

(if (and r1 r2 r3)
    (print "SUCCESS")
    (print "FAILURE"))
