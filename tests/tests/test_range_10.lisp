
(define eerr '(exit-error eval_error))

(define r1 (eq eerr (trap (range))))
(define r2 (eq eerr (trap (range 'apa))))
(define r3 (eq eerr (trap (range 'apa 1))))
(define r4 (eq eerr (trap (range 1 'apa))))
(define r5 (eq nil (range  1 1)))


(check (and r1 r2 r3 r4 r5))
