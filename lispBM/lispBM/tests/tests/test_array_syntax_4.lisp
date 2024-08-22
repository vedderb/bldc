(define arr [1 2 3 4 5 6])


(check (and (= 1 (bufget-u8 arr 0))
            (= 6 (bufget-u8 arr 5))))
