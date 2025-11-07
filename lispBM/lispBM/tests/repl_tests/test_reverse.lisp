

(define r1 (reverse (list 1 2 3)))
(define r2 (trap (reverse 'apa)))
(define r3 (trap (reverse [| 1 2 3 |])))
(define r4 (trap (reverse [1 2 3])))
(define r5 (trap (reverse)))

(print r2 "\n" r3 "\n" r4 )

(if (and (eq r1 (list 3 2 1))
         (eq r2 '(exit-error eval_error))
         (eq r3 '(exit-error eval_error))
         (eq r4 '(exit-error eval_error))
         (eq r5 '(exit-error eval_error)))
    (print "SUCCESS")
    (print "FAILURE"))
         
