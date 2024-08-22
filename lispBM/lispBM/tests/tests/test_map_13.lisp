

(defun f (y)
  (let ( (x 100))
    (+ y x)))


(def f1 (lambda (ls) (map f ls)))


(check (eq (f1 (list 1 2 3)) (list 101 102 103)))
