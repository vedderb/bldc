(define dec-cnt3 (lambda (x)
  (if (> x 0) (dec-cnt3 (- (- (- (- (- (- (- x 1) 1) 1) 1) 1) 1) 1)) 0)
))

(dec-cnt3 100000)
