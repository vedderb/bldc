
(define r1 (list? nil))
(define r2 (list? (list 1 2 3 )))
(define r3 (list? '(1 2 3)))
(define r4 (not (list? 'apa)))
(define r5 (not (list? 1)))

(check (and r1 r2 r3 r4 r5))
