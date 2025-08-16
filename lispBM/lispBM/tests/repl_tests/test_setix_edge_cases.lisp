

(define r1 (eq (setix (list 1 2 3 4 5) -1 0) (list 1 2 3 4 0)))
(define r2 (eq (trap (setix [|1 2 3|]  5 0)) '(exit-error type_error)))
(define r3 (eq (trap (setix 'apa  5 0)) '(exit-error type_error)))


(if (and r1 r2 r3)
    (print "SUCCESS")
    (print "FAILURE"))
