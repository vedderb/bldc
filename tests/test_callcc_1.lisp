
(define f (lambda (x) (+ x 1)))



(= (f (call-cc (lambda (k) (k 10)))) 11)
