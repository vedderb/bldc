(define r1 (eq '(exit-error type_error) (trap (list-to-array 'apa))))
(define r2 (eq '(exit-error type_error) (trap (list-to-array (list 1 2 3) 75))))
(define r3 (eq nil (list-to-array nil)))

(check (and r1 r2 r3))

