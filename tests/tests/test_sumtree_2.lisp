(define is-number
  (lambda (x)
    (or (eq (type-of x) type-i)
	(eq (type-of x) type-u)
	(eq (type-of x) type-float)
	(eq (type-of x) type-i32)
	(eq (type-of x) type-u32))
    ))
	    
      
(define sumtree
  (lambda (x)
    (if (is-number x)
	x
      (if (eq x 'nil)
	  0
	(let ((a (sumtree (car x)))
	      (b (sumtree (cdr x))))
	  (+ a b)
	  )))))


(check (= (sumtree (list (list 1u32 1i32 1u 1 1 1 1 1 1 1) (list 1 2.0) (list 3 4.0))) 20.0))
