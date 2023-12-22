
(define a '(1))
(define b '(2))

(define cmp (lambda (x y) (< x y)))

(check (eq (merge cmp a b) '(1 2)))
