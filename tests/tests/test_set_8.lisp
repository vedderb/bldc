;; Wrong number or type of args tests
(define r1 (eq '(exit-error type_error) (trap (set-union))))
(define r2 (eq '(exit-error type_error) (trap (set-union 1))))
(define r3 (eq '(exit-error type_error) (trap (set-union (list 1 2 3)))))
(define r4 (eq '(exit-error type_error) (trap (set-union (list 1 2 3) 1))))
(define r5 (eq '(exit-error type_error) (trap (set-union (list 1 2 3) (list 4 5 6) (list 4 5 6)))))

(check (and r1 r2 r3 r4 r5))
