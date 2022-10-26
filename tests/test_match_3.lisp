
(define f (lambda (ls)
	    (match ls
		   ( nil 0 )
		   ( ( (? c) . (? cd)) (+ c (f c)))
		   ( _ 'error-not-a-list))))

(eq (f 'kurt) 'error-not-a-list)
