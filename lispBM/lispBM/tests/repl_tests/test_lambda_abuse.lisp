
(define a1  (lambda))
 
(define a2 (lambda [ 1 2 3 ]))
(define a3 (lambda [| 1 2 3 |]))
(define a4 (trap (lambda (x) . [ 1 2 3 ])))
(define a5 (trap (lambda (x) . [| 1 2 3 |])))
(define a6 (trap (lambda (x) . 3.14f32)))
(define a7 (trap (lambda (x) . 3.14f64)))
(define a8 (trap (lambda (x) . "apa")))

(define a9 (lambda (x . [ 1 2 3 ])))
(define a10 (lambda (x . [| 1 2 3 |])))


(a1)
(trap (a2))
(trap (a2 1))
(trap (a2 1 2))
(trap (a2 1 2 3))
(trap (a3))
(trap (a3 1))
(trap (a3 1 2))
(trap (a3 1 2 3))

(trap (a9 1))
(trap (a9 1 2))
(trap (a10 1))
(trap (a10 1 2))

;; Run GC as a way to see if heap is in ok state.
(gc)


;; test is successful if there is no crash
(print "SUCCESS")
