
(define f (lambda (x)
	    (match x ( (apa (? x)) (+ x 10))
		     ( (? x) x))))

(check (= (f '(apa 10)) 20))
