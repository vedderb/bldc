

(define r1 (eq '(exit-error eval_error) (trap (bufcreate 1 2))))
(define r1 (eq '(exit-error eval_error) (trap (bufcreate 1 'apa))))

(if r1
    (print "SUCCESS")
    (print "FAILURE"))
