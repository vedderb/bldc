
(define b (bufcreate 16))

(free b)

(gc)

(define r (+ 1 2))

(define r1 (eq '(exit-error type_error) (trap (bufset-i8 b 0 1))))

(check (and (= r 3) r1))
