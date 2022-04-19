(define arr [type-i32 0 1 2 3 4 5 6 7 8 9 10 11])

(and (= (array-read arr 0) 0)
     (= (array-read arr 7) 7)
     (= (array-read arr 11) 11))
