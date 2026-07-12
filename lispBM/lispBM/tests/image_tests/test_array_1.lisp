(define a [| 1 2 3|])

(defun main () {
  (if (eq a [| 1 2 3 |])
      (print "SUCCESS")
    (print "FAILURE"))
  })

(image-save)
(f-write-image (f-open "image.lbm" "w"))
