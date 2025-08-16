(define empty-array [||])

(defun main ()
  (if (eq (length empty-array) 0)
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(fwrite-image (fopen "image.lbm" "w"))