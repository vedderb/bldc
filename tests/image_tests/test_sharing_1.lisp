(define a (list 1 2 3))

(define b (cons 100 a))

(defun main () {
       (if (and (eq a (list 1 2 3))
                (eq b (list 100 1 2 3 )))  
           (print "SUCCESS")
         (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
