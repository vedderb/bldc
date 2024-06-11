

(define a (match (trap (/ 1 (match (trap (/ 1 0))
                                   ( (exit-error (? err)) 0)
                                   ( (exit-ok    (? v)))  1)))
                 ( (exit-error (? err)) 100)
                 ( (exit-ok    (? v))   0)))

(check (= a 100))
