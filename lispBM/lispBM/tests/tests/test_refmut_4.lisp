
(define a 1)

(define r (setcdr a 999))

(check (and (not r) (= a 1)))
