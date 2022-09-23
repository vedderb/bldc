(namespace myspace
	   (let ((n 1000))
	     (defun f (x) (+ x n))))


(= (myspace:f 1) 1001)
