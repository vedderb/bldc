
(define f (lambda (ls)
	    (match ls
		   ( nil 0 )
		   ( ((? x) . (? xs)) (+ x (f xs)))
		   ( _ 'error-not-a-list))))

(check (= (f '(1 2 3 4)) 10))
