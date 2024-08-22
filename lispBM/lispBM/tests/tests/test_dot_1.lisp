
(define x (vector 1 2 3))
(define y (vector 1 5 7))

(define r (dot x y))

(check (= r 32.0))
