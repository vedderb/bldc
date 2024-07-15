
(defun f (x)
  (map (lambda (y) (+ x y)) (rest-args) ))


(check (eq (f 1 1 1) '(2 2)))
