
(define r1 (trap (map (lambda (x) (+ x 1)) 'apa)))
(define r2 (trap (map (lambda (x) (+ x 1)) [| 1 2 3 |])))
(define r3 (trap (map)))

(define r4 (eq (map (lambda (x) (+ x 1)) (list 1 2 3)) (list 2 3 4)))
(define r5 (eq (map (lambda (x) (print x)) (list 1 2 3 4 5)) (list t t t t t)))

(if (and (eq r1 '(exit-error eval_error))
         (eq r2 '(exit-error eval_error))
         (eq r3 '(exit-error eval_error))
         r4 r5)
    (print "SUCCESS")
    (print "FAILURE")
    )
