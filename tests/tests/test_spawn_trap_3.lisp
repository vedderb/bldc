
(define t1 (lambda (x)
             (apa)))



(spawn-trap t1 50)

(check (eq (recv ((exit-error (? tid) (? e)) e)
                 ((exit-ok    (? tid) (? r)) r))
           variable_not_bound))
