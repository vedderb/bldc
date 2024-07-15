
(defun f (x) (+ x 1))

(defun g (x) (check (= (f x) (+ x 1))))

(spawn 20 g 100)
