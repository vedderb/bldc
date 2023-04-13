(check (let ((f (lambda () (lambda (x) (+ x 1)))))
         (= ((f) 1) 2)))
