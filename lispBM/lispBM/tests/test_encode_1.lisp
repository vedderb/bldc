
(define close-enough
    (lambda (x y)
      (if (> x y)
          (< (- x y) 0.0001)
          (< (- y x) 0.0001)
          )))

(close-enough 3.14 (encode-float (decode 3.14)))


