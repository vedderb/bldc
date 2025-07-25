
(define terr '(exit-error type_error))


(define r1 (eq terr (trap (take))))
(define r2 (eq terr (trap (drop))))
(define r3 (eq terr (trap (take (list 1 2 3) 'apa))))
(define r4 (eq terr (trap (drop (list 1 2 3) 'bepa))))
(define r5 (eq terr (trap (take 'apa 10))))
(define r6 (eq terr (trap (drop 'bepa 10))))

(check (and r1 r2 r3 r4 r5 r6))


