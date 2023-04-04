
(define a 1)

(define b 2)

(define f (lambda () (+ a b)))

(check (= (f) 3))

