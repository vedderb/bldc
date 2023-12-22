
(define a '(1 3))
(define b '(2 4))

(define cmp (lambda (x y) (< x y)))

(check (eq (merge cmp a b) '(1 2 3 4)))
