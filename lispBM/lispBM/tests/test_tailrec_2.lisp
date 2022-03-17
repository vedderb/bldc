(define sum (lambda (s rest) (if (eq rest nil) s (sum (+ s (car rest)) (cdr rest)))))

( = (sum 0 (list 2.0 2.0 1.0 1.0 3.0 3.0)) 12.0)
