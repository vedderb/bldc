(define pi 3.14159265359)
(define neg-float -123.456)
(define zero-float 0.0)

(defun main ()
  (if (and (eq pi 3.14159265359)
           (eq neg-float -123.456)
           (eq zero-float 0.0))
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(fwrite-image (fopen "image.lbm" "w"))