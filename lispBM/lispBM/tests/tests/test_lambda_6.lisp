(define f (lambda ()
            (lambda (x)
              (+ x 1))))

(check (= ((f) 1) 2))
