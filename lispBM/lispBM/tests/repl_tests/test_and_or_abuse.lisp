

(define r1 (and t . 1))
(define r2 (or  t . 1))
(define r3 (eq (trap (and . 1)) '(exit-error type_error)))
(define r4 (eq (trap (or . 1)) '(exit-error type_error)))

(if (and r1 r2 r3 r4)
    (print "SUCCESS")
    (print "FAILURE"))
