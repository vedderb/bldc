
(define r1 (trap (not)))
(define r2 (trap (not 1 2)))

(check (and (eq '(exit-error eval_error) r1)
            (eq '(exit-error eval_error) r2)))
