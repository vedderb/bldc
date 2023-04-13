

(define x (vector 1.0 2.0 3.0))
(define y (vector 0.1 0.2 0.3))
(define alpha 2)

(define r (axpy alpha x y))

(check (eq (vector-to-list r) (list 2.1 4.2 6.3)))
