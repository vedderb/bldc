
(define a 0)


(loop ( (i 10) )
      (> i 0)
      {
      (var b (+ a i))
      (setq a b)
      (setq i (- i 1))
      })

(check (= a 55))
