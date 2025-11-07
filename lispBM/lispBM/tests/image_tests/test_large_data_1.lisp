(define big-list (range 1 1000))
(define big-array (apply array (range 1 100)))

(defun main ()
  (if (and (eq (length big-list) 999)
           (eq (length big-array) 99)
           (eq (car big-list) 1)
           (eq (ix big-array 0) 1))
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(fwrite-image (fopen "image.lbm" "w"))