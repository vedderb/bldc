
(define a [| 1 2 3 |])

(define b [| apa bepa cepa |])

(check (and (= (ix a 0) 1)
            (= (ix a 1) 2)
            (= (ix a 2) 3)
            (eq (ix b 0) 'apa)
            (eq (ix b 1) 'bepa)
            (eq (ix b 2) 'cepa)))
