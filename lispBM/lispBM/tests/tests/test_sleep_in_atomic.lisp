
(define err '(exit-error eval_error))

(define r (eq err (trap (atomic (sleep 1)))))

(check r)
