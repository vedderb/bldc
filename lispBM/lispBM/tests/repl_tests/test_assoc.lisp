
(define a (assoc nil 1))
(define e1 (trap (assoc 'apa 1)))
(define e2 (trap (assoc 1 1)))
(define e3 (trap (assoc nil)))
(define e4 (trap (assoc)))



(if (and (eq '(exit-error eval_error) e1)
         (eq '(exit-error eval_error) e2)
         (eq '(exit-error eval_error) e3)
         (eq '(exit-error eval_error) e4)
         (eq a nil))
    (print "SUCCESS")
    (print "FAILURE"))

