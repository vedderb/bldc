

(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))


;; Possibly odd behavior that (eval-program 'apa) => apa
(define r1 (eq 'apa (eval-program 'apa)))
(define r2 (eq 1 (eval-program 1)))
(define r3 (eq '(exit-error eval_error) (trap (eval-program 1 2))))
(define r4 (eq '(exit-error eval_error) (trap (eval-program 1 2 3))))



(if (and r1 r2 r3 r4)
    (print "SUCCESS")
    (print "FAILURE"))
