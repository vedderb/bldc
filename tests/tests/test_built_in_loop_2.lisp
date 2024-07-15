
(define a 0)

(loop ( (v '(1 2 3 4 5 6 7 8 9 10)) )
      v
      {
      (setq a (+ (car v) a))
      (setq v (cdr v))
      })


(check (= a 55))
