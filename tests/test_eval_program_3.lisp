(define prg (list (eval-program (list (+ 1 2) (+ 2 3) (+ 10 5)))))


(= (eval-program prg) 15)
