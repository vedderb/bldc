
(define eerr '(exit-error eval_error))
(define terr '(exit-error type_error))

;; it is possible to call sym2u with an arbitrary u
;; but it is strongly adviced against.
(define sym-u (sym2u 'bepa))

(define r1 (eq (u2sym (sym2u 'apa)) 'apa))
(define r2 (eq (sym2u (u2sym sym-u)) sym-u))

(define r3 (eq eerr (trap (sym2u))))
(define r4 (eq terr (trap (sym2u 23))))

(define r5 (eq eerr (trap (u2sym))))
(define r6 (eq terr (trap (u2sym 'apa))))

(check (and r1 r2 r3 r4 r5 r6))
