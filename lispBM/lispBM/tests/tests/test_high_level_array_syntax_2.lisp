(define a [| 1 2 3 |])
(define b [| (1 2 3) (4 5 6) |])
(define c [| (1 . 2) (3 . 4) |])
(define d [| apa bepa cepa depa |])

(check (and (= (ix a 0) 1)
            (eq (ix b 0) '(1 2 3))
            (eq (ix c 0) '(1 . 2))
            (eq (ix d 0) 'apa)))
