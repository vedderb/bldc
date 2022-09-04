(define foo (lambda ()
              (atomic (+ 1 2 3))))

(= (foo) 6)
           
