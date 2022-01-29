(define f (lambda (ls)
	    (match ls
		   ( nil 0 )
		   ( (?cons c) (+ (car c) (f (cdr c))))
		   ( ? 'error-not-a-list))))

(= (f '()) 0)
