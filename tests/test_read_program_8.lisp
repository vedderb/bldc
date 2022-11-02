(defun f (x)
   (+ x (eval-program (read-program "(+ 100 150)"))))


(and (= (f 1) 251)
     (= (f 2) 252))
