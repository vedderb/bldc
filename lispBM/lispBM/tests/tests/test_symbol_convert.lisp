
(define eerr '(exit-error eval_error))
(define terr '(exit-error type_error))

(define r1 (eq (str2sym (sym2str 'apa)) 'apa))
(define r2 (eq (sym2str (str2sym "bepa")) "bepa"))

(define r3 (eq eerr (trap (sym2str))))
(define r4 (eq terr (trap (sym2str 23))))

(define r5 (eq eerr (trap (str2sym))))
(define r6 (eq terr (trap (str2sym 23))))

(check (and r1 r2 r3 r4 r5 r6))
