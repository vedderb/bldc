
(define a 1)

(define r (setcdr a 999))

(and (not r) (= a 1))
