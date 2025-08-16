
(define r1 (trap (apply)))
(define r2 (trap (apply 1)))

(if (and (eq r1 '(exit-error eval_error))
         (eq r2 '(exit-error eval_error)))
    (print "SUCCESS")
    (print "FAILURE"))
