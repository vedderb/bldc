
(define f (lambda (x)
            (if (num-eq x 0)
                'done
              (progn
                (gc)
                (f (- x 1))))))


(= (f 100) 'done)
              
