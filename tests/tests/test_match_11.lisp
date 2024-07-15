
(defun f (x) "apa")

(check (= (match (f 0)
                 ("bepa" 0)
                 ("eepa" 1)
                 ("kurt" 2)
                 ("apa"  3)
                 ("hej"  4))
          3))
 
