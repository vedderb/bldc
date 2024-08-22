
(define test (lambda ()
	       (if (ext-numbers 1)
		   (progn
		     (define a (+ 1 2))
		     (define b (+ 2 3))
		     (define c (+ 3 4))
		     (+ a b c)
		     ))))

(check (= (test) 15))
