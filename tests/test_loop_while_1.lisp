
(define n 0)

(= (loopwhile (<= n 10)
              (define n (+ n 1)))
   11)
