
(define fred (lambda ()
               (progn (print "fred iteration\n" )
                      (recv ((apa (? x)) (print "fred received: " x "\n")))
                      (fred))))

(define bella (lambda (pid x)
                (progn (print "bella iteration " x "\n")
                       (send pid '(bepa ,x))
                       (yield 50000)
                       (bella pid (+ x 1)))))

(define fredpid (spawn fred))

(spawn bella (car fredpid) 0)
