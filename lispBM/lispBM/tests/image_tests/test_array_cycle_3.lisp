(define arr [| 1 2 [| a b c |] 3 4|])

(setix arr 2 (setix (ix arr 2) 2 arr))

(defun main () {
       (if (and (= (ix arr 0) 1)
                (= (ix arr 1) 2)
            )
            (print "SUCCESS")
            (print "FAILURE"))
         })
       
(image-save)
(fwrite-image (fopen "image.lbm" "w"))
