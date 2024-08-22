
(define f (lambda (x) (+ x 1)))



(check (= (f (call-cc (lambda (k) (k 10)))) 11))
