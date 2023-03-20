

(defun t1 ()
  (progn
    1
    (var x 10)
    2
    (var y 5)
    (+ x y)))

(= (t1) 15)
