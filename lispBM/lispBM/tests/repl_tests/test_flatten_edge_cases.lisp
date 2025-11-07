
(define r1 (trap (unflatten)))
(define r2 (trap (unflatten 'apa)))
(define r3 (trap (unflatten 'apa 'bepa)))
(define r4 (trap (unflatten [| 1 2 3 4 |])))

(if (and (eq r1 '(exit-error type_error))
         (eq r2 '(exit-error type_error))
         (eq r3 '(exit-error type_error))
         (eq r4 '(exit-error type_error))
         )
    (print "SUCCESS")
    (print "FAILURE"))
