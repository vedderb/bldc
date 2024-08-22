
(define fred (lambda ()
	       (progn (print "fred iteration\n" )
                      (yield 25000)
		      (fred))))

(define bella (lambda (x)
		(progn (print "bella iteration" x "\n")
                       (yield 50000)
		       (bella (+ x 1)))))


(spawn fred)
(spawn bella 0)
