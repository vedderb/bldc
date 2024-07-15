
(defun f (x) (+ x 1))

(defun g (x) (check (= (f x) (+ x 1))))

(spawn g 100)
