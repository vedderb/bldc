
(define err_val '(exit-error eval_error))

(define r1 (eq err_val (trap (recv))))
(define r2 (eq err_val (trap (recv-to))))
(define r3 (eq err_val (trap (recv-to 1))))

(check (and r1 r2 r3))
