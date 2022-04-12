
(define fred (lambda ()
               (progn (print "fred iteration" \#newline )
                      (recv ((apa (? x)) (print "fred received: " x \#newline)))
                      (fred))))

(define bella (lambda (pid x)
                (progn (print "bella iteration " x \#newline)
                       (send pid '(bepa ,x))
                       (yield 50000)
                       (bella pid (+ x 1)))))

(define fredpid (spawn '(fred)))

(spawn '(bella (car fredpid) 0))
