
(define arr [| 1 2 3 |])

(setix arr 2 arr)

(defun main () {
       (if (and (= (ix arr 0) 1)
                (= (ix arr 1) 2)
                (= (ix (ix arr 2) 0) 1)
            )
            (print "SUCCESS")
            (print "FAILURE"))
         })
       
(image-save)
(fwrite-image (fopen "image.lbm" "w"))
