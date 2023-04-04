(define sumtree
  (lambda (x)
    (if (eq (type-of x) type-i)
	x
      (if (eq x 'nil)
	  0
	(let ((a (sumtree (car x)))
	      (b (sumtree (cdr x))))
	  (+ a b)
	  )))))


(check (= (sumtree (list (list 1 1 1 1 1 1 1 1 1 1) (list 1 2) (list 3 4))) 20))
