(define foo (lambda ()
              (atomic (+ 1 2 3))))

(check (= (foo) 6))
           
