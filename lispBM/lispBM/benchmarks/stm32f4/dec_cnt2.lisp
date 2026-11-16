(define dec-cnt2 (lambda (x)
  (match x (0 0) (_ (dec-cnt2 (- x 1))))
))

(dec-cnt2 100000)
