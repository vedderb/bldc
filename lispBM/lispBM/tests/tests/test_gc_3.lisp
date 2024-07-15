
(define a (list 1 2 3 4))
(define b (list a 'x 'y 'z))
(define c (list b 'x1 'y1 'z1))

(gc)

(check (and
        (eq a (list 1 2 3 4))
        (eq b (list (list 1 2 3 4) 'x 'y 'z))
        (eq c (list (list (list 1 2 3 4) 'x 'y 'z) 'x1 'y1 'z1))))
