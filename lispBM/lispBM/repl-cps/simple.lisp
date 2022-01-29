
(define fred (lambda ()
	       (progn (print "fred iteration" \#newline )
                      (yield 25000)
		      (fred))))

(define bella (lambda (x)
		(progn (print "bella iteration" x \#newline)
                       (yield 50000)
		       (bella (+ x 1)))))


(spawn '(fred) '(bella 0))
