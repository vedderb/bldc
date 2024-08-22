(check (let ((f (lambda (x) (if (= x 0) 0 (f (- x 1))))))
         (= (f 2) 0)))

