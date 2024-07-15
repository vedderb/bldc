
(define a '(2 4))
(define b '())

(define cmp (lambda (x y) (< x y)))

(check (eq (merge cmp a b) '(2 4)))
