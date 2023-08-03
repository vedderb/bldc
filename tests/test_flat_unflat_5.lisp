
(define tree '(("hello" "kurt") ("russel" "rules")))

(define a (flatten tree))

(check (eq (unflatten a) tree))
