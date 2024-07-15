
(define arr (bufcreate 16))

(free arr)

(check (and (eq (car arr) nil)
            (eq (cdr arr) nil)))
