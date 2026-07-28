(define test-lisp-array [| 1 2 3|])

(defun main ()
  (if (eq (length test-lisp-array) 3)
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(f-write-image (f-open "image.lbm" "w"))
