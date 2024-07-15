
(defun f (x)
  (+ 1 x))

(defun g (y)
  (+ (f 0) (f y)))


(define res
  (match (trap (g 100))
         ( (exit-error (? err)) 1000)
         ( (exit-ok     (? v))   v)
         ))

(check (= res 102))
