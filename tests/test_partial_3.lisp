(define f (lambda (x y z w) (+ x y z w)))

(define g (f 1 2))

(= (g 3 4) 10)
