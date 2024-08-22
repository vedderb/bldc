
(define f (lambda (x) (+ x 2)))

(check (eq (match (f 1)
                  ( 3  't)
                  ( _  'nil))
           't))
