
(define test (lambda (x)
	       (if x
		   'something)))

(and (eq (test 't) 'something)
     (eq (test 'nil) 'nil))
