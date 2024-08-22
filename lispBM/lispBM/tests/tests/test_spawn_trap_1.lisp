

(define t1 (lambda (x)
             (+ x 100)))



(spawn-trap t1 50)

(check (= (recv ((exit-error (? tid) (? e)) 'error)
                ((exit-ok    (? tid) (? r)) r))
          150))
