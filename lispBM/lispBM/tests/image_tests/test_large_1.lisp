

(define a (range 100))
(define b (range 100))
(define c (range 100))

(defun main () {
       (if (and (eq (ix a 15) 15)
                (eq (ix b 77) 77)
                (eq (ix c 43) 43))
           (print "SUCCESS")
         (print "FAILURE"))
       })

(image-save)
(fwrite-image (fopen "image.lbm" "w"))
