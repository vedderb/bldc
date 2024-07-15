
(defun f ()
  (let (( a {1 2 3 4 5 6 7} ))
    (+ a 100)))

(check (= (f) 107))
