
(define f (lambda (x y)
            (+ x y)))

(define r1 (eq '(exit-error eval_error) (trap (apply 1 (list 1 2 3 4)))))
(define r2 (eq '(exit-error eval_error) (trap (apply '(apa bepa cepa)  (list 1 2 3 4)))))
(define r3 (eq '(exit-error eval_error) (trap (apply f  (list 1)))))

(if (and r1 r2 r3)
    (print "SUCCESS")
    (print "FAILURE"))
