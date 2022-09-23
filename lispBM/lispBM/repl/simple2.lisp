
(define fred (lambda ()
               (progn (print "fred iteration" \#newline )
                      (recv ((? x) (print "fred received: " x \#newline)))
                      (fred))))

(define bella (lambda (pid x)
                (progn (print "bella iteration " x \#newline)
                       (send pid x)
                       (yield 500000)
                       (bella pid (+ x 1)))))

(define fredpid (spawn '(fred)))

(spawn '(bella (car fredpid) 0))
