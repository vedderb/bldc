
(define n 0)

(check (= (loopwhile (<= n 10)
                     (define n (+ n 1)))
          11))
