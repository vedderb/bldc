
(define r1 (eq t (and)))
(define r2 (eq nil (or)))

(check (and r1 r2))
