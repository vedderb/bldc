
(define f2 (lambda () (lambda (x) (- x 1))))

(define f (lambda (n) ((f2) n) ))

(check (= (f 2) 1))
