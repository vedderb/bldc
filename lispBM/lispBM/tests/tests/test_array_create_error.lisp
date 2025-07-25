
(define r1 (trap (array-create)))
(define r2 (trap (array-create (list 1 2 3))))
(define r3 (trap (array-create 1 2)))

(check  (and (eq '(exit-error eval_error) r1)
             (eq '(exit-error eval_error) r2)
             (eq '(exit-error eval_error) r3)))
