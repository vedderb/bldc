(define mixed [| 1 3.14 "hello" nil |])

(defun main ()
  (if (and (eq (ix mixed 0) 1)
           (eq (ix mixed 1) 3.14)
           (eq (ix mixed 2) "hello")
           (eq (ix mixed 3) nil))
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(fwrite-image (fopen "image.lbm" "w"))