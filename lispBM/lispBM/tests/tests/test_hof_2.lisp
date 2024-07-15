(check (let ((f (lambda (y) (lambda (x) (+ x y)))))
         (=((f 2) 1) 3)))

