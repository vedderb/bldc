(define empty-array [||])

(defun main ()
  (if (eq (length empty-array) 0)
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(f-write-image (f-open "image.lbm" "w"))