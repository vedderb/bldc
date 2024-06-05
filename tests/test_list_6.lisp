
(define f (lambda () (list 1 2 3 4)))

(setix (f) 2 77)

(check (eq (f) (list 1 2 3 4)))

