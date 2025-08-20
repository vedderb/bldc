
(define r1 (array-create 0))
(define r2 [])

(if (and (eq r1 []) (eq r2 []))
    (print "SUCCESS")
    (print "FAILURE"))
