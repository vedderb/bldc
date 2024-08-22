

(define a (cons 1 2))

(setcdr a 199)

(check (and (= (car a) 1) (= (cdr a) 199)))

