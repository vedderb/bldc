(defun miter (cr ci zr zi n)                                                  
  (if (or (> (+ (* zr zr) (* zi zi)) 4.0) (= n 0))
      n                                                                       
    (miter cr ci
           (+ (- (* zr zr) (* zi zi)) cr)
           (+ (* 2.0 zr zi) ci)
           (- n 1))))

(defun ch (n)
  (if (= n 0) 32
    (if (< n 4) \#.
      (if (< n 8) \#:
        (if (< n 14) \#+
          (if (< n 20) \#* \##))))))

(defun row (ci) {
       (var buf (bufcreate 55))
       (loopfor i 0 (< i 54) (+ i 1)
                (bufset-u8 buf i (ch (miter (+ -2.0 (* i 0.05)) ci 0.0 0.0 24))))
       (print buf)
       })

(defun mandel (y)
  (if (> y 21) t
    (progn
      (row (+ -1.05 (* y 0.1)) 0)
      (mandel (+ y 1)))))

(mandel 0)
