

(defun f (n)
  (if (= n 0) 'yay
    {
    1
    2
    3
    4
    5
    (f (- n 1))
    }
    ))


(check (eq (f 100) 'yay))
