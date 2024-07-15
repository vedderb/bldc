
(defun f ()
  (progn
    (var a 10)
    (var b (+ a 10))
    (+ a b)))

(check (= (f) 30))
