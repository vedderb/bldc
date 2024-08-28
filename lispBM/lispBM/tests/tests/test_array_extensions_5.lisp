
(define arr (bufcreate 16))

(free arr)

(check (eq '(exit-error type_error) (trap (bufset-i8 arr 0 1))))
