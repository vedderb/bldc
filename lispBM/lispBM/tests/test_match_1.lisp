
(define f (lambda (x)
	    (match x ( (apa (? x)) (+ x 10))
		     ( (? x) 10))))

(= (f '(foo 3)) 10)
