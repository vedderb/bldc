


(define t1 (lambda ()
             (append '(1 2 3) 'hej '(4 5 6))))


(spawn-trap t1 50)

(check (eq (recv ((exit-error (? tid) (? e)) e)
                 ((exit-ok    (? tid) (? r)) r))
           type_error))
