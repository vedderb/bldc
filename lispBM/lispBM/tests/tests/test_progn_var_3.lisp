

(defun f ()
  (progn
    (var a 10)
    (var b 20)
    (+ a b)))

(check (= (f) 30))
