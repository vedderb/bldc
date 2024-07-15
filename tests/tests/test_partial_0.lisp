;; Partial application has been removed

(define f (lambda (x y z w) (+ x y z w)))

(check (= (f 1 2 3 4) 10))


