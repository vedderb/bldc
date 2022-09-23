
(define f (lambda (x)
	    (match x ( (apa (? x)) (+ x 10))
		     ( (? x) x))))

(= (f '(apa 10)) 20)
