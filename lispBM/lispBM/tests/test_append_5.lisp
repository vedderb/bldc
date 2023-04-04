

(define t1 (lambda ()
             (append 1)))


(spawn-trap t1 50)

(check (eq (recv ((exit-error (? tid) (? e)) e)
                 ((exit-ok    (? tid) (? r)) r))
           type_error))
           
