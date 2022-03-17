
(define fred (lambda ()
	       (progn (print "fred iteration" \#newline )
		      (recv ( (apa (? x) 107)  (print "fred received apa " x \#newline))
			    ( (bepa (?i28 x))  (print "fred received bepa " x \#newline)))
		      (fred))))

(define bella (lambda (pid x)
		(progn (print "bella iteration" x \#newline)
		       (send pid `(apa ,x 107))
		       (yield 500000)
		       (print "bella waking up" \#newline)
		       (send pid '(bepa 2))
		       (yield 500000)
		       (bella pid (+ x 1)))))

(define fredpid (spawn '(fred)))

(spawn '(bella (car fredpid) 0))

