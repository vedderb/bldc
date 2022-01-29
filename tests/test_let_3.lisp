(let ((x 0)
      (f (lambda (y) (+ y x)))
      (g (lambda (x) (+ x 1))))
  (= (list (f 10) (g 10)) '(10 11)))

