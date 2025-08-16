

(define r1 (eq (trap (+ 1 2 3 'apa 4 5 6)) '(exit-error type_error)))
(define r2 (eq (trap (- 1 2 3 'apa 4 5 6)) '(exit-error type_error)))
(define r3 (eq (trap (* 1 2 3 'apa 4 5 6)) '(exit-error type_error)))
(define r4 (eq (trap (/ 1 2 3 'apa 4 5 6)) '(exit-error type_error)))
(define r5 (eq (trap (// 1 'apa)) '(exit-error type_error)))

(if (and r1 r2 r3 r4 r5)
    (print "SUCCESS")
    (print "FAILURE"))
