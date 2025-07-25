
(define apa (list 1 2 3))

(define r1 (trap (mod 1 'apa)))
(define r2 (trap (mod 'apa 1)))
(define r3 (trap (mod 'apa)))
(define r4 (trap (mod (list 1 2 3))))
(define r5 (trap (mod 1 apa)))
(define r6 (trap (mod apa 1)))
(define r7 (trap (mod 1 0)))

                 
(check (and (eq '(exit-error type_error) r1)
            (eq '(exit-error type_error) r2)
            (eq '(exit-error eval_error) r3)
            (eq '(exit-error eval_error) r4)
            (eq '(exit-error type_error) r5)
            (eq '(exit-error type_error) r6)
            (eq '(exit-error division_by_zero) r7)))
