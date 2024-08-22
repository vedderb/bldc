(define arr [0 1 2 3 4 5 6 7 8 9 10 11])

(check (and (= (bufget-u8 arr 0) 0)
            (= (bufget-u8 arr 7) 7)
            (= (bufget-u8 arr 11) 11)))
