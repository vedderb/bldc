
(define test (lambda (f x)
               (if (f x)
                   x
                   (test f (- x 1)))))
               
(check (= (test (lambda (x) (< x 0)) 2) -1))
