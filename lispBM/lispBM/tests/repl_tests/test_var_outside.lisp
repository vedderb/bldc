
(trap (var a 10))


(define e1 (trap (var a 10)))


(if (eq '(exit-error eval_error) e1)
    (print "SUCCESS")
    (print "FAILURE")
    )
