
(define r1 (let ((a1 10)) {
                (var a2 20)
                (let ((a3 30)) {
                     (var a4 40)
                     (let ((a5 50)) {
                          (var a6 60)
                          (let ((a7 70)) {
                               (var a8 80)
                               (let ((a9 90))
                                 (+ a1 a2 a3 a4 a5 a6 a7 a8 a9)
                               })
                          })
                     })
                     })))

(define b1 (= r1 450))
             




(if (and b1)
    (print "SUCCESS")
  (print "FAILURE"))

