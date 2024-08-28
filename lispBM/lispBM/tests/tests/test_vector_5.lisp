

(def v (list-to-vector '(1.0 2.0 3.0 4.0)))

(custom-destruct v)

(define r (eq '(exit-error type_error) (trap (mag v))))

(check r)
