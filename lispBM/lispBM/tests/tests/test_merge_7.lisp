(define a '(1))
(define b '(2 3 4 5 6))

(define cmp (lambda (x y) (< x y)))

(check (eq (merge cmp a b) '(1 2 3 4 5 6)))
