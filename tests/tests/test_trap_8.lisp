
(define a (match (trap (/ 4 (match (trap (+ 1 1))
                                   ( (exit-error (? err)) 0)
                                   ( (exit-ok    (? v))  v))))
                 ( (exit-error (? err)) 100)
                 ( (exit-ok    (? v))   v)))

(check (= a 2))
