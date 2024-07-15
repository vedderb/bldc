(check (let ((f (lambda (x) (+ x 1)))
             (g (lambda (h) (h 1))))
         (= (g f) 2)))
       
