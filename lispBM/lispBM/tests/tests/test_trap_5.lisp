
(defun f (x)
  (match (trap (/ 1 0))
         ( (exit-error (? err)) 1000)
         ( (exit-ok    (? v))   v)))


(defun g (y)
  (+ (f 0) (f y)))


(define res (g 100))
  

(check (= res 2000))
