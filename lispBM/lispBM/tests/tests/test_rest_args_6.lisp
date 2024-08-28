
(defun f ()
  (+ 1 (rest-args 2) (rest-args 4)))

(check (= (f 1 2 3 4 5 6) 9))
