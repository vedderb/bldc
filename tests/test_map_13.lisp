

(defun f (y)
  (let ( (x 100))
    (+ y x)))


(def f1 (map f))


(check (eq (f1 (list 1 2 3)) (list 101 102 103)))
