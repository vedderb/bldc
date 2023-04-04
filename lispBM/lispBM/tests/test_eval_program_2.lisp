(define prg '( (eval-program '( (+ 1 2) (+ 2 3) (+ 10 5)))))


(check (= (eval-program prg) 15))
