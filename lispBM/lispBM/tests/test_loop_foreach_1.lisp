(define n 0)

(loopforeach i '(0 1 2 3 4 5 6 7 8 9 10)
             (define n (+ n i)))

(check (eq n 55))
