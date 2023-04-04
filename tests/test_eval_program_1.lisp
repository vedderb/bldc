
(define prg '( (+ 1 2) (+ 3 4) (+ 10 5)))


(check (= (eval-program prg) 15))
