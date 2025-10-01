

(def a '(+ 1 0))
(def b `(+ ,a ,a))

(defun f (x) (eval `(+ x ,b)))

(setix a 2 2)

(defun main ()
  (if (eq (f 1) 7)
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
