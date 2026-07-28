
(define a (list 1 2 3 4))


(defun main ()       
  (if (eq a (list 1 2 3 4)) (print "SUCCESS") (print "FAILURE"))
  )



(image-save)
(f-write-image (f-open "image.lbm" "w"))
