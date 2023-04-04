(check (progn
         (define f (lambda (x)
                     (if (= x 0)
                         't
                       (f (- x 1)))))
         (f 1)))
