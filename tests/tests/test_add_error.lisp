
(define apa (list 1 2 3))

(define r1 (trap (+ 1 'apa)))
(define r2 (trap (+ 'apa 1)))
(define r3 (trap (+ 'apa)))
(define r4 (trap (+ (list 1 2 3))))
(define r5 (trap (+ 1 apa)))
(define r6 (trap (+ apa 1)))

                 
(check (and (eq '(exit-error type_error) r1)
            (eq '(exit-error type_error) r2)
            (eq '(exit-error type_error) r3)
            (eq '(exit-error type_error) r4)
            (eq '(exit-error type_error) r5)
            (eq '(exit-error type_error) r6)))
