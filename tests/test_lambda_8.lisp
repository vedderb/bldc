(define f (lambda ()
            (let ((a 1))
              (lambda (x)
                (let ((b a)) 
                  (+ x b))))))

(check (= ((f) 1) 2))
