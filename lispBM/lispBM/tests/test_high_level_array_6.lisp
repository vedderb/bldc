
(define a (list-to-array (list 1 2 3)))
(define b (list-to-array (list 3 2 1)))

(check (not (eq a b)))
