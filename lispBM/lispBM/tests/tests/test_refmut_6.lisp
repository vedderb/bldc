(define a (cons 1 2))

(setcdr a (cons 7 8))

(check (eq a (cons 1 (cons 7 8))))
