
(define f
  (lambda (x)
    (if (= x 0)
	0
      (+ x (f (- x 1))))))

(check (= (progn
            (define a 5)
            (f a)
            (define a 4)
            (f a)
            )
          10))
