
(defun f (x) "kurt")

(= (match (f 0)
          ("bepa" 0)
          ("eepa" 1)
          ("kurt" 2)
          ("apa"  3)
          ("hej"  4))
   2)
