
(define a 1)

(define r (set-car a 999))

(and (not r) (= a 1))
