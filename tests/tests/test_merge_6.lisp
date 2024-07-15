(define a '())
(define b '(1 3))

(define cmp (lambda (x y) (< x y)))

(check (eq (merge cmp a b) '(1 3)))
