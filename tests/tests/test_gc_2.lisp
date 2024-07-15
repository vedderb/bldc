
(define a (list 1 2 3 4))
(define b (list a 'x 'y 'z))

(gc)

(check (and
        (eq a (list 1 2 3 4))
        (eq b (list (list 1 2 3 4) 'x 'y 'z))))
