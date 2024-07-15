
@const-start
(define f (lambda (ls)
	    (match ls
		   ( nil 0 )
		   ( ( (? c) . (? cd)) (+ c (f c)))
		   ( _ 'error-not-a-list))))
@const-end

(check (and (eq (f 'kurt) 'error-not-a-list)
            (eq (f nil) 0)))
