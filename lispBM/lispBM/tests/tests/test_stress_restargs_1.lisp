
;; a bit busy as a test but it triggers the restargs gc issue. 


(define f (lambda (x y z) {
	    (var a 1)
	    (+ a x y z (rest-args 0) (rest-args 1))
	    }))




(define test (lambda (n)
	       (if (= n 0) (f 1 2 3 1 1 0 0 0 0 0)
		   {
		   (let ((arg0 1)
			 (arg1 2)
			 (arg2 3)
			 (arg3 4)
			 (arg4 5))
		     (f arg0 arg1 arg2 arg3 arg4 1 2 3 4 5)
		     )
		   (test (- n 1))
		   })))


(check (= (test 1000) 9))
		   
	       
