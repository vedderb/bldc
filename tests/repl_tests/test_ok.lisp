
(define f (lambda () (exit-ok 10)))
(define g (lambda () (exit-ok)))

(define h (lambda () (exit-error 'apa)))

(define cid1 (spawn-trap f))

(define r1 (recv ((? x) x)))

(define cid2 (spawn-trap g))

(define r2 (recv ((? x) x)))

(define cid3 (spawn-trap h))

(define r3 (recv ((? x) x)))

(print r3)

(if (and (eq r1 `(exit-ok ,cid1 10))
         (eq r2 `(exit-ok ,cid2 t))
         (eq r3 `(exit-error ,cid3 apa)))
    (print "SUCCESS")
    (print "FAILURE")
    )
