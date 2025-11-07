(define e1 (trap (match)))

(define r1 (eq (match 1) no_match))
(define r2 (eq (match 1 nil) no_match))

(print r1 r2)

(if (and (eq '(exit-error eval_error) e1)
         r1 r2)
    (print "SUCCESS")
    (print "FAILURE"))
