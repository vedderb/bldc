(define apa '(1 2 3 4 5 6 7 8 9 10))


(check (and (= (ix apa -10) 1)
            (= (ix apa 0) 1)
            (= (ix apa -9) 2)
            (= (ix apa 1) 2)))
