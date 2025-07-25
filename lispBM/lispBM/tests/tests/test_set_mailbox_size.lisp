

(define eerr '(exit-error eval_error))
(define terr '(exit-error type_error))

(define r1 (eq eerr (trap (set-mailbox-size))))
(define r2 (eq eerr (trap (set-mailbox-size 1 2))))
(define r3 (eq terr (trap (set-mailbox-size 'apa))))
(define r4 (eq nil  (set-mailbox-size 128000)))


(check (and r1 r2 r3 r4))
