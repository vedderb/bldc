
(defun f (a)
  `(+ b ,a))


(let ((b 101))
  (and (= (eval (f 1)) 102)
       (= (eval (f 100)) 201)))
  
