(check (let ((f (lambda (x) (if (= x 0) 0 (g (- x 1)))))
             (g (lambda (x) (if (= x 0) 1 (f (- x 1))))))
         (= (f 10) 0)))
