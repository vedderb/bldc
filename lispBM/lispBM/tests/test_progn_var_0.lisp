

(defun t1 ()
  (progn
    1
    (var x 10)
    2
    (var y 5)
    (+ x y)))

(check (= (t1) 15))
