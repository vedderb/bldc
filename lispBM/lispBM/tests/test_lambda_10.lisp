
(define f (lambda ()
            (lambda (n)
              (if (= n 0)
                  42
                  ((f) (- n 1))
                  ))))


(= ((f) 1) 42)
