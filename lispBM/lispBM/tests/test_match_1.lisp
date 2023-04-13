
(define f (lambda (x)
	    (match x ( (apa (? x)) (+ x 10))
		     ( (? x) 10))))

(check (= (f '(foo 3)) 10))
