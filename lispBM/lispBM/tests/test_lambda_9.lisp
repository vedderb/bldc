(define f (lambda ()
            (let ((a 1))
              (let ((b 2))
                (lambda (x)
                  (let ((b a))
                    (let ((c 3))
                      (+ x b))))))))

(= ((f) 1) 2)
