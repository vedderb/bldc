(define short-str "hello")
(define long-str "this is a much longer string with various characters !@#$%")
(define empty-str "")

(defun main ()
  (if (and (eq short-str "hello")
           (eq long-str "this is a much longer string with various characters !@#$%")
           (eq empty-str ""))
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(fwrite-image (fopen "image.lbm" "w"))