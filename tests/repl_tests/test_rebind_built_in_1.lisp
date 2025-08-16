

(define r1 (trap (let ((+ 1)) t)))
(define r2 (trap (let (([1 2 3] 1)) t)))

(if (and (eq r1 '(exit-error eval_error))
         (eq r2 '(exit-error eval_error)))
    (print "SUCCESS")
    (pring "FAILURE"))

 
