(define a (list 1 2 3 4 5))

(defun main () {
  (if (eq a (list 1 2 3 4 5))
      (print "SUCCESS")
    (print "FAILURE"))
  })

(image-save)
(f-write-image (f-open "image.lbm" "w"))
