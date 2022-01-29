
(define a 1)

(define r (set-cdr a 999))

(and (not r) (= a 1))
