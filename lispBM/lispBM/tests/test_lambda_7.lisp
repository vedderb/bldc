(define f (lambda ()
            (let ((a 1))
              (lambda (x)
                (+ x a)))))

(= ((f) 1) 2)
