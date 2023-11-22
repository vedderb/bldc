
(define a 0)

(loop ( (i 0) )
      ( <= i 10 )
      {
      (setq a (+ a i) )
      (setq i (+ i 1) )
      })


(check (= a 55))
