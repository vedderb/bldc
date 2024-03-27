
(defun f (x)
  (+ x (rest-args 2)))

(check (= (f 1 2 3 4 5 6) 5))
