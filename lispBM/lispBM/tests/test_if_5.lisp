

(define test (lambda (x)
	       (progn
		 (define a (+ 1 2 3 4 5))
		 (if x
		     (progn
		       (define b 10)
		       (+ a b))))))

(check (and (eq (test 't) 25)
            (eq (test 'nil) 'nil)))
