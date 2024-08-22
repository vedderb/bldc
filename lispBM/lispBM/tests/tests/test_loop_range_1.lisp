
(define n 0)


(looprange i 0 11
               (define n (+ n i)))

(check (eq n 55))
