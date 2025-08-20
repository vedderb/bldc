
(define r1 (trap (exit-error 'apa)))
(define r2 (trap (exit-error)))
(define r3 (trap (exit-error 'apa 'bepa)))

(if (and (eq r1 '(exit-error apa))
         (eq r2 '(exit-error eval_error))
         (eq r3 '(exit-error apa)))
    (print "SUCCESS")
    (print "FAILURES")
    )
