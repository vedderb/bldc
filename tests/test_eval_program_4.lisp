(define prg (list (eval-program (list (+ 1 2) (+ 2 3) (+ 10 5)))))
(define r (+ 100 (eval-program prg)))


(= (eval-program prg) 15)
