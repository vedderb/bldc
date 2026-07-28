(defun add (x y) (+ x y))
(defun mul (x y) (* x y))
(defun complex-calc (a b c) (add (mul a b) c))

(defun main ()
  (if (eq (complex-calc 2 3 4) 10)
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(f-write-image (f-open "image.lbm" "w"))