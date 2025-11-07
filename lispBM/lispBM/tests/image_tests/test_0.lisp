
(define a (list 1 2 3 4))


(defun main ()       
  (if (eq a (list 1 2 3 4)) (print "SUCCESS") (print "FAILURE"))
  )



(image-save)
(fwrite-image (fopen "image.lbm" "w"))
