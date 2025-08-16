(define nested (list (list 1 2) [| 3 4 |] (list [| 5 6 |] 7)))

(defun main ()
  (if (and (eq (car (car nested)) 1)
           (eq (ix (car (cdr nested)) 1) 4)
           (eq (ix (car (car (cdr (cdr nested)))) 0) 5))
      (print "SUCCESS")
      (print "FAILURE")))

(image-save)
(fwrite-image (fopen "image.lbm" "w"))