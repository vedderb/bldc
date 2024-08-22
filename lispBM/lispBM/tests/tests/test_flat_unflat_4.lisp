
(define tree '((1 2) (3 4)))

(define a (flatten tree))

(check (eq (unflatten a) tree))
