(define f (lambda ()
            (let ((a 1))
              (lambda (x)
                (let ((b a)) 
                  (+ x b))))))

(= ((f) 1) 2)
