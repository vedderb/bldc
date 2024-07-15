
(define n 0)


(check (= (loopfor i 0 (<= i 10) (+ i 1)
                   (define n (+ n i)))
          55))
