
(define f (lambda (x)
            (if (= x 0)
                'done
              (progn
                (gc)
                (f (- x 1))))))


(eq (f 100) 'done)
              
