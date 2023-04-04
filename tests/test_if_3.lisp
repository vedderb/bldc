
(define test (lambda (x)
	       (if x
		   'something)))

(check (and (eq (test 't) 'something)
            (eq (test 'nil) 'nil)))
