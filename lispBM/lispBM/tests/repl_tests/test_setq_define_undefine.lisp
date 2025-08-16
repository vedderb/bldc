

(trap (setq 'apa 10))
(define apa 10)
(undefine 'apa)

(define r1 (trap apa))

(if (eq r1 '(exit-error variable_not_bound))
    (print "SUCCESS")
    (print "FAILURE"))
