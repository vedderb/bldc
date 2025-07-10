(define a [| 1 2 3 |])
(define b [| (1 2 3) (4 5 6) |])
(define c [| (1 . 2) (3 . 4) |])
(define d [| apa bepa cepa depa |])

(define n_a (read (to-str a)))
(define n_b (read (to-str b)))
(define n_c (read (to-str c)))
(define n_d (read (to-str d)))

(check (and (eq n_a a)
            (eq n_b b)
            (eq n_c c)
            (eq n_d d)))
