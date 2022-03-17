
(define f (lambda (ls)
	    (match ls
		   ( nil 0 )
		   ( (?cons c) (+ (car c) (f (cdr c))))
		   ( _ 'error-not-a-list))))

(eq (f 'kurt) 'error-not-a-list)
