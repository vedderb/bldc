

(define a (match (trap (/ 1 0))
                 ( (exit-error (? err)) 10)
                 ( (exit-ok    (? v  )) v)))

(check (= a 10))
