
(define t1 (lambda (x)
             (read-program)))



(spawn-trap t1 50)

(check (eq (recv ((exit-error (? tid) (? e)) e)
                 ((exit-ok    (? tid) (? r)) r))
           eval_error))
       
