(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))



(define r1 (eq '(exit-error read_error) (trap (read "`,@(list 1 2 3)"))))

(debug_test r1 1)

(if (and r1)
    (print "SUCCESS")
    (print "FAILURE"))
