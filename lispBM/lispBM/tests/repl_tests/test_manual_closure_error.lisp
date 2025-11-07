
(define clo (closure (x) (+ a x) (a . 1)))

(define err (trap (clo 2)))


(if (eq err '(exit-error variable_not_bound))
    (print "SUCCESS")
    (print "FAILURE")
    )
