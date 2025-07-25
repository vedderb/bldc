(define a '(1 2 3 5 6))
(define b '(4 7))


(check (eq (merge < a b) '(1 2 3 4 5 6 7)))
