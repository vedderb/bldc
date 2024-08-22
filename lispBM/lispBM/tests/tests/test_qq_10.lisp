
(let ((b 101))
  (defun f (a)
    `(+ ,b ,a)))


(check (and (= (eval (f 1)) 102)
            (= (eval (f 100)) 201)))
