

(define a (cons 1 2))
(define b (cons 3 4))

(define tree (cons a b))

(gc)

(check (eq tree '( (1 . 2) . (3 . 4) )))

