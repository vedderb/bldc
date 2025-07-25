
(define eerr '(exit-error eval_error))
(define terr '(exit-error type_error))

(define r1 (eq eerr (trap (length))))
(define r2 (eq terr (trap (length 'apa))))


(check (and r1 r2))
