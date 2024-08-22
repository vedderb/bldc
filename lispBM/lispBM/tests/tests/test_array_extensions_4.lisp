
(define close-enough
    (lambda (x y)
      (if (> x y)
          (< (- x y) 0.0001)
          (< (- y x) 0.0001)
          )))

(define arr (bufcreate 16))

(bufset-f32 arr 0 3.14)
(bufset-f32 arr 4 666.666)
(bufset-f32 arr 8 100)
(bufset-f32 arr 12 42)

(check (and (close-enough (bufget-f32 arr 0) 3.14)
            (close-enough (bufget-f32 arr 4) 666.666)
            (close-enough (bufget-f32 arr 8) 100)
            (close-enough (bufget-f32 arr 12) 42)))
