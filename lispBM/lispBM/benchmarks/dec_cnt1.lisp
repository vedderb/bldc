(define dec-cnt (lambda (x)
  (if (= x 0) 0 (dec-cnt (- x 1)))
  ))


(dec-cnt 100000)
