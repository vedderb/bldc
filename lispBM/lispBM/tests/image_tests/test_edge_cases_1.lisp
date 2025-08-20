(define empty-list '())
(define empty-array (array))
(define single-item (list 42))

(defun main ()
  (if (and (eq empty-list '())
           (eq (length empty-array) 0)
           (eq (car single-item) 42))
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(fwrite-image (fopen "image.lbm" "w"))