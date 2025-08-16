

(defun f (x) (+ x 1))

(define add1 '(+ 1 2))

(defun main () {
       (if (and (eq (f 1) 2)
                (eq (eval add1) 3))  
           (print "SUCCESS")
         (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
