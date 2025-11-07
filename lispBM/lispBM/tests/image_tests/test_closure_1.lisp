(define x 42)
(define f (lambda (y) (+ x y)))

(defun main ()
  (if (eq (f 8) 50)
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(fwrite-image (fopen "image.lbm" "w"))