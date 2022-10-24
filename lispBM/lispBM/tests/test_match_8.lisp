
(define f (lambda (x) (+ x 2)))

(eq (match (f 1)
          ( 3  't)
          ( _  'nil))
   't)
