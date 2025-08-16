
(define r1 (eq '(exit-error eval_error) (trap (setcar 0))))
(define r2 (eq '(exit-error eval_error) (trap (setcdr 0))))

(if (and r1 r2)
    (print "SUCCESS")
    (print "FAILURE"))
