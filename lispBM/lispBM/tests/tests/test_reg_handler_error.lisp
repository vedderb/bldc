
(define terr '(exit-error type_error))

(define r1 (eq terr (trap (event-register-handler))))
(define r2 (eq terr (trap (event-register-handler 'apa))))

(check (and r1 r2))
