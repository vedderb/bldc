

(define t1 (lambda (x)
             (+ x 100 'apa)))



(spawn-trap t1 50)

(check (eq (recv ((exit-error (? tid) (? e)) e)
                 ((exit-ok    (? tid) (? r)) r))
           type_error))
