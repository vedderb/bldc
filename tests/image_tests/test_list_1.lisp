(define a (list 1 2 3))

(defun main () {
  (if (eq a (list 1 2 3))
      (print "SUCCESS")
    (print "FAILURE"))
  })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
