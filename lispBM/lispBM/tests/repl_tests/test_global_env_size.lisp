
(define s0 (global-env-size))
(define apa 10)
(define s1 (global-env-size))

(if (= s1 (+ 2 s0))
    (print "SUCCESS")
    (print "FAILURE"))
