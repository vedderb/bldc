(define q2 (lambda (x y)
  (if (or (< x 1) (< y 1)) 1
    (+ (q2 (- x (q2 (- x 1) y)) y)
       (q2 x (- y (q2 x (- y 1))))))))

(q2 6 7)
