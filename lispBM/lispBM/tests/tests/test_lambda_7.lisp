(define f (lambda ()
            (let ((a 1))
              (lambda (x)
                (+ x a)))))

(check (= ((f) 1) 2))
