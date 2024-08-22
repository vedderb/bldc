(define foo (lambda ()
              (atomic (progn (+ 1 2 3)
                             (+ 1 2 4)
                             (+ 2 4 8)
                             (+ 3 6 12)))))

(check (= (foo) 21))
