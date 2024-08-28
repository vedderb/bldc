(define err '(exit-error eval_error))

(define is-error (lambda (x) (eq x err)))

(define r1 (is-error (trap (acons))))
(define r2 (is-error (trap (acons 1))))
(define r3 (is-error (trap (acons 1 2 3 4))))

(check (and r1 r2 r3))
