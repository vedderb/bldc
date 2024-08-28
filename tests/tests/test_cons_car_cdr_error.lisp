(define eerr '(exit-error eval_error))
(define terr '(exit-error type_error))

(define r1 (eq eerr (trap (cons))))
(define r2 (eq eerr (trap (cons 1))))
(define r3 (eq eerr (trap (cons 1 2 3))))

(define r4 (eq eerr (trap (car))))
(define r5 (eq eerr (trap (cdr))))
(define r6 (eq eerr (trap (car 1 2))))
(define r7 (eq eerr (trap (cdr 1 2))))

(define r8 (eq terr (trap (car 1))))
(define r9 (eq terr (trap (cdr 1))))


(check (and r1 r2 r3 r4 r5 r6 r7 r8 r9))
