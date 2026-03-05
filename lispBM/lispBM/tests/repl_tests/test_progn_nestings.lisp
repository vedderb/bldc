
(define r1 { (var a1 10)
           { (var a2 20)
           { (var a3 30)
           { (var a4 40)
           { (var a5 50)
           { (var a6 60)
           { (var a7 70)
           { (var a8 80)
           { (var a9 90)
           { (+ a1 a2 a3 a4 a5 a6 a7 a8 a9) }
           }}}}}}}}})

(define r2 { (var a1 10)
           { (var a2 (+ a1 10))
           { (var a3 (+ a2 10))
           { (var a4 (+ a3 10))
           { (var a5 (+ a4 10))
           { (var a6 (+ a5 10))
           { (var a7 (+ a6 10))
           { (var a8 (+ a7 10))
           { (var a9 (+ a8 10))
           { (+ a1 a2 a3 a4 a5 a6 a7 a8 a9) }
           }}}}}}}}})


(if (and (= r1 450)
         (= r2 450))
    (print "SUCCESS")
    (print "FAILURE"))
