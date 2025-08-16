(define a (list 1 2 3))

(define b (cons 100 a))

(defun main () {

       (setix b 1 100)
       (setix b 2 200)
       (setix b 3 300)
       
       (if (and (eq a (list 100 200 300))
                (eq b (list 100 100 200 300 )))  
           (print "SUCCESS")
         (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
