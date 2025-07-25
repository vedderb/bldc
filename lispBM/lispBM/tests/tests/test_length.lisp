
(define r1 (= (length (list 1 2 3)) 3))
(define r2 (= (length [1 2 3]) 3))
(define r3 (= (length (list-to-array (list 1 2 3))) 3))

(check (and r1 r2 r3))
