(define apa '(1 2 3 4 5 6 7 8 9 10))


(check (and (= (ix apa -6) 5)
            (= (ix apa 4) 5)
            (= (ix apa -4) 7)
            (= (ix apa 6) 7)))
