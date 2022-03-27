
(define f (lambda ()
            (let ((x 200))
              (let ((y 100))
                (lambda (n)
                  (let ((a 4))
                    (let ((b 5))
                      (let ((c 6))
                        (if (= n 0) (+ x y a b c) (f (- n 1)))))))))))


(define g (f))

(= ((g) 10) 315)
