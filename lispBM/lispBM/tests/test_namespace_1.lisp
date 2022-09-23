
(namespace n
	   (progn
	     (define a1 1)
	     (define a2 2)
	     (define a3 3)
	     (define a4 4)
	     (define a5 5)))

(define a1 1000)
(define a2 2000)
(define a3 3000)
(define a4 4000)
(define a5 5000)

(and (= a1 1000)
     (= n:a1 1)
     (= a2 2000)
     (= n:a2 2)
     (= a3 3000)
     (= n:a3 3)
     (= a4 4000)
     (= n:a4 4)
     (= a5 5000)
     (= n:a5 5))
