
(define a (list 1 2 3 4))

(defun main ()       
  (print "FAILURE")
  )

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
