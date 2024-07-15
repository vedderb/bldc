
(defun f (a)
  (let ((b 101))
    `(+ ,b ,a)))


(check (and (= (eval (f 1)) 102)
            (= (eval (f 100)) 201)))
