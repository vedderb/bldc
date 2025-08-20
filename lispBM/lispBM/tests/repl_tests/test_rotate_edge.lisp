(define r1 (trap (rotate 'apa 1)))
(define r2 (trap (rotate (list 1 2 3) 'apa)))
(define r3 (trap (rotate)))
(define r4 (trap (rotate 1 2 3)))


(if (and (eq r1 '(exit-error eval_error))
         (eq r2 '(exit-error eval_error))
         (eq r3 '(exit-error eval_error))
         (eq r4 '(exit-error eval_error)))
    (print "SUCCESS")
    (print "FAILURE"))
