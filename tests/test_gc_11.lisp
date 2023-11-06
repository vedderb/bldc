
(define v (list 1 2 3 4))
(define fv (flatten v))

(gc)

(check (and
        (eq (unflatten fv) (list 1 2 3 4))
        (eq v (list 1 2 3 4))))
