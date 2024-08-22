(check (let ((f (lambda (x) (if (= x 0) 0 (+ x (f (- x 1))))))) (= (f 10) 55)))

