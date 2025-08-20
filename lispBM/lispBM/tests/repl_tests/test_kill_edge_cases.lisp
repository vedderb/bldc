
(define r1 (trap (kill)))
(define r2 (trap (kill 'apa)))
(define r3 (kill 1 1)) ; made up nonexistent thread

(print r3)

(if (and (eq r1 '(exit-error type_error))
         (eq r2 '(exit-error type_error))
         (eq r3 nil))
    (print "SUCCESS")
    (print "FAILURE"))
