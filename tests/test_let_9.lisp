
(check (let ((f (lambda (x)
                  (let ((f (lambda (x) (+ x 10))))
                    (f 100)))))
         (= (f 0) 110)))
